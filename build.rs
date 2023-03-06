//! Generate code based on the schema.
//!
//! The code generated is:
//! 1. SQ's internal representation of the schema.
//! 1. Traits for each SQ type type that specify the methods they must implement.
//! 1. An implementation of the `SqValue` trait for each SQ type that dispatches dynamic field
//!    calls (with the name of the field to call as a string from the query) to concrete functions
//!    provided by the type (which are specified by the traits mentioned in #2).

use std::{env, fmt, fs, fs::File, io::BufReader, path::Path};

use anyhow::{anyhow, Result};
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use serde::{de, Deserialize, Deserializer};
use serde_json::Value as JsonValue;

/// Represents the external schema (the JSON schema, not the internal representation of the
/// schema used by the SQ binary).
#[derive(Debug, Deserialize)]
#[must_use]
struct ExtSchema {
    root_type: String,
    types: Vec<TypeExt>,
}

/// Represents an SQ type in the external schema.
#[derive(Debug, Deserialize)]
#[must_use]
struct TypeExt {
    name: String,
    doc: DocExt,
    primitive_coercion: String,
    fields: Vec<FieldExt>,
}

/// Represents a field of an SQ type in the external schema.
#[derive(Debug, Deserialize)]
#[must_use]
struct FieldExt {
    name: String,
    doc: DocExt,
    params: Vec<ParamExt>,
    return_type: String,
    return_sequence_type: ReturnSequenceType,
}

/// Represents whether a field call returns a single value, an optional value, or a sequence of
/// values.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[must_use]
enum ReturnSequenceType {
    Single,
    Option,
    Sequence,
}

impl<'de> Deserialize<'de> for ReturnSequenceType {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        match s.as_str() {
            "Single" => Ok(Self::Single),
            "Option" => Ok(Self::Option),
            "Sequence" => Ok(Self::Sequence),
            other => Err(de::Error::custom(format!(
                "Invalid return_sequence_type {}",
                other
            ))),
        }
    }
}

impl fmt::Display for ReturnSequenceType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Single => write!(f, "Single"),
            Self::Option => write!(f, "Option"),
            Self::Sequence => write!(f, "Sequence"),
        }
    }
}

/// Represents a field call parameter in the external schema.
#[derive(Debug, Deserialize)]
#[must_use]
struct ParamExt {
    name: String,
    doc: DocExt,
    index: usize,

    #[serde(rename = "type")]
    ty: String, // `type` is a Rust keyword so don't use that here.

    required: bool,
    default_value: JsonValue,
}

/// Represents the documentation for an item in the external schema.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
#[must_use]
enum DocExt {
    String(String),
    // The external schema splits up long pieces of documentation into arrays of lines.
    Vec(Vec<String>),
}

/// Get a Rust path to an SQ type (e.g. `sqbool::SqBool`).
fn sq_type_str_to_path(type_str: &str) -> TokenStream2 {
    let type_ident = format_ident!("{}", type_str);
    let mod_ident = format_ident!("{}", type_str.to_lowercase());
    quote! {crate::system::#mod_ident::#type_ident}
}

/// Generates SQ's internal schema representation from the external schema.
struct SchemaGenerator<'es> {
    ext_schema: &'es ExtSchema,
}

impl<'es> SchemaGenerator<'es> {
    pub fn new(ext_schema: &'es ExtSchema) -> Self {
        Self { ext_schema }
    }

    /// Generate the internal schema representation.
    pub fn generate(&mut self) -> Result<TokenStream2> {
        let type_schemas: Vec<TokenStream2> = self
            .ext_schema
            .types
            .iter()
            .map(|te| self.generate_for_type(te))
            .collect::<Result<Vec<TokenStream2>>>()?;

        let root_type_name = &self.ext_schema.root_type;

        Ok(quote! {
            static TYPES: crate::schema::TypeMap = phf_map!{#(#type_schemas),*};
            static ROOT_TYPE_NAME: &str = #root_type_name;
        })
    }

    /// Generate the internal schema representation of an SQ type.
    fn generate_for_type(&mut self, type_ext: &TypeExt) -> Result<TokenStream2> {
        let name = &type_ext.name;
        let doc = Self::quote_doc(&type_ext.doc);
        let primitive_coercion = self.quote_primitive_coercion(type_ext)?;

        let field_schemas: Vec<TokenStream2> = type_ext
            .fields
            .iter()
            .map(|fe| self.generate_for_field(type_ext, fe))
            .collect::<Result<Vec<TokenStream2>>>()?;

        Ok(quote! {
            #name => TypeSchema {
                name: #name,
                doc: #doc,
                primitive_coercion: #primitive_coercion,
                fields: phf_map!{#(#field_schemas),*},
            }
        })
    }

    /// Generate the internal schema representation of a field of an SQ type.
    fn generate_for_field(
        &mut self,
        type_ext: &TypeExt,
        field_ext: &FieldExt,
    ) -> Result<TokenStream2> {
        let name = &field_ext.name;
        let doc = Self::quote_doc(&field_ext.doc);
        let return_type = &field_ext.return_type;
        let return_sequence_type = format_ident!("{}", &field_ext.return_sequence_type.to_string());
        let parent_type = &type_ext.name;

        let param_schemas: Vec<TokenStream2> = field_ext
            .params
            .iter()
            .map(|pe| self.generate_for_param(type_ext, field_ext, pe))
            .collect::<Result<Vec<TokenStream2>>>()?;

        Ok(quote! {
            #name => FieldSchema {
                name: #name,
                doc: #doc,
                parent_type: #parent_type,
                return_type: #return_type,
                return_sequence_type: crate::fieldcall::FieldSequenceType::#return_sequence_type,
                params: phf_ordered_map!{#(#param_schemas),*},
            }
        })
    }

    /// Generate the internal schema representation of a parameter of a field of an SQ type.
    fn generate_for_param(
        &mut self,
        type_ext: &TypeExt,
        field_ext: &FieldExt,
        param_ext: &ParamExt,
    ) -> Result<TokenStream2> {
        let index = param_ext.index;
        let name = &param_ext.name;
        let doc = Self::quote_doc(&param_ext.doc);
        let ty = self.quote_param_type(type_ext, field_ext, param_ext)?;
        let required = param_ext.required;
        let default = self.quote_param_default_value(type_ext, field_ext, param_ext)?;

        Ok(quote! {
            #name => ParamSchema {
                index: #index,
                name: #name,
                doc: #doc,
                ty: #ty,
                required: #required,
                default: #default,
            }
        })
    }

    /// Generate the internal schema representation of the documentation for an item.
    fn quote_doc(doc_ext: &DocExt) -> TokenStream2 {
        let doc_str = match doc_ext {
            DocExt::String(s) => s.clone(),
            DocExt::Vec(v) => v.join(" "),
        };
        quote! {#doc_str}
    }

    /// Generate the internal schema representation of a type of SQ primitive.
    fn quote_primitive_type(name: &str) -> Option<TokenStream2> {
        match name {
            "PrimitiveBool" => Some(quote! {crate::primitive::PrimitiveKind::Bool}),
            "PrimitiveInt" => Some(quote! {crate::primitive::PrimitiveKind::Int}),
            "PrimitiveFloat" => Some(quote! {crate::primitive::PrimitiveKind::Float}),
            "PrimitiveString" => Some(quote! {crate::primitive::PrimitiveKind::Str}),
            _ => None,
        }
    }

    /// Generate the internal schema representation of the SQ primitive type that an SQ value can
    /// be coerced to.
    fn quote_primitive_coercion(&self, type_ext: &TypeExt) -> Result<TokenStream2> {
        Self::quote_primitive_type(type_ext.primitive_coercion.as_str()).ok_or_else(|| {
            anyhow!(
                "schema error in type {}: invalid primitive_coercion {}",
                type_ext.name,
                type_ext.primitive_coercion
            )
        })
    }

    /// Generate the internal schema representation of the SQ primitive type of a field parameter.
    fn quote_param_type(
        &self,
        type_ext: &TypeExt,
        field_ext: &FieldExt,
        param_ext: &ParamExt,
    ) -> Result<TokenStream2> {
        Self::quote_primitive_type(param_ext.ty.as_str()).ok_or_else(|| {
            anyhow!(
                "schema error at {}::{} param {}: invalid param type {}",
                type_ext.name,
                field_ext.name,
                param_ext.name,
                param_ext.ty.as_str(),
            )
        })
    }

    /// Generate the internal schema representation of a default value for a parameter of a field.
    fn quote_param_default_value(
        &self,
        type_ext: &TypeExt,
        field_ext: &FieldExt,
        param_ext: &ParamExt,
    ) -> Result<TokenStream2> {
        match (param_ext.ty.as_str(), &param_ext.default_value) {
            (_, JsonValue::Null) => Ok(quote! {DefaultValue::Null}),
            ("PrimitiveBool", JsonValue::Bool(b)) => Ok(quote! {DefaultValue::Bool(#b)}),
            ("PrimitiveInt", JsonValue::Number(n)) if n.is_i64() => {
                let i = n.as_i64().unwrap();
                Ok(quote! {DefaultValue::Int(#i)})
            }
            // Note that serde_json::value::Number::is_f64() returns true only if the number is not
            // representable as a u64 or i64 so it wouldn't do what we want here.
            ("PrimitiveFloat", JsonValue::Number(n)) if n.as_f64().is_some() => {
                let f = n.as_f64().unwrap();
                Ok(quote! {DefaultValue::Float(#f)})
            }
            ("PrimitiveString", JsonValue::String(s)) => Ok(quote! {DefaultValue::Str(#s)}),
            (tn, v) => Err(anyhow!(
                "schema error at {}::{} param {}: default value {:?} is not a valid {:?}",
                &type_ext.name,
                &field_ext.name,
                &param_ext.name,
                v,
                tn
            )),
        }
    }
}

/// Generates traits specifying the methods that each SQ type must implement.
struct SqTypeTraitGenerator<'es> {
    ext_schema: &'es ExtSchema,
}

impl<'es> SqTypeTraitGenerator<'es> {
    pub fn new(ext_schema: &'es ExtSchema) -> Self {
        Self { ext_schema }
    }

    /// Generate traits for each SQ type.
    pub fn generate(&mut self) -> TokenStream2 {
        let field_traits: Vec<TokenStream2> = self
            .ext_schema
            .types
            .iter()
            .map(|ty| self.generate_sq_type_trait(ty))
            .collect();

        quote! {#(#field_traits)*}
    }

    /// Generate the trait for a particular SQ type.
    fn generate_sq_type_trait(&mut self, type_ext: &TypeExt) -> TokenStream2 {
        let trait_name = format_ident!("{}Trait", &type_ext.name);
        let fields: Vec<TokenStream2> = type_ext
            .fields
            .iter()
            .map(|field| self.generate_trait_method(field))
            .collect();
        quote! {
            pub trait #trait_name: crate::sqvalue::SqValue {
                fn to_primitive(&self) -> anyhow::Result<crate::primitive::Primitive>;
                #(#fields)*
            }
        }
    }

    /// Generate the method declaration for a field of an SQ type.
    fn generate_trait_method(&mut self, field_ext: &FieldExt) -> TokenStream2 {
        let method_name = format_ident!("{}", &field_ext.name);
        let base_return_type = sq_type_str_to_path(&field_ext.return_type);
        let return_type = match field_ext.return_sequence_type {
            ReturnSequenceType::Single => base_return_type,
            ReturnSequenceType::Option => quote! {Option<#base_return_type>},
            ReturnSequenceType::Sequence => {
                quote! {crate::sqvalue::SqValueSequence<#base_return_type>}
            }
        };

        let params: Vec<TokenStream2> = field_ext
            .params
            .iter()
            .map(|param| self.generate_trait_method_param(param))
            .collect();

        quote! {
            fn #method_name(&self, #(#params)*) -> anyhow::Result<#return_type>;
        }
    }

    /// Generate a param specification for a method of an SQ type.
    fn generate_trait_method_param(&mut self, param_ext: &ParamExt) -> TokenStream2 {
        let param_name = format_ident!("p_{}", &param_ext.name);
        let mut param_type = match &param_ext.ty[..] {
            "PrimitiveBool" => quote! {bool},
            "PrimitiveInt" => quote! {i64},
            "PrimitiveFloat" => quote! {f64},
            "PrimitiveString" => quote! {&str},
            // The internal schema generation should already have errored if the type is invalid so
            // don't worry too much about an informative message.
            s => panic!("invalid param type {}", s),
        };

        if !param_ext.required {
            param_type = quote! {Option<#param_type>};
        }

        quote! {#param_name: #param_type,}
    }
}

/// Implements `SqValue` for each SQ type. This involves turning field calls in the query/AST into
/// concrete method calls on SQ types.
struct SqValueImplementor<'es> {
    ext_schema: &'es ExtSchema,
}

impl<'es> SqValueImplementor<'es> {
    pub fn new(ext_schema: &'es ExtSchema) -> Self {
        Self { ext_schema }
    }

    /// Generate the `SqValue` implementation for each SQ type.
    pub fn generate(&mut self) -> TokenStream2 {
        let call_impls: Vec<TokenStream2> = self
            .ext_schema
            .types
            .iter()
            .map(Self::generate_impl)
            .collect();

        quote! {#(#call_impls)*}
    }

    /// Generate the `SqValue` implementation for a particular Sq type.
    fn generate_impl(type_ext: &TypeExt) -> TokenStream2 {
        let type_name = &type_ext.name;
        let type_path = sq_type_str_to_path(&type_ext.name);

        let get_single_impl = Self::generate_get_impl(
            type_ext,
            quote! {get_single},
            ReturnSequenceType::Single,
            quote! {crate::error::Result<crate::sqvalue::SqBValue>},
            quote! {|field| -> crate::sqvalue::SqBValue { Box::new(field) }},
        );

        let get_option_impl = Self::generate_get_impl(
            type_ext,
            quote! {get_option},
            ReturnSequenceType::Option,
            quote! {crate::error::Result<Option<crate::sqvalue::SqBValue>>},
            quote! {|opt_field| -> Option<crate::sqvalue::SqBValue> {
                opt_field.map(|f| -> SqBValue { Box::new(f) })
            }},
        );

        let get_sequence_impl = Self::generate_get_impl(
            type_ext,
            quote! {get_sequence},
            ReturnSequenceType::Sequence,
            // See the definition of 'a in generate_get_impl().
            quote! {crate::error::Result<crate::sqvalue::SqBValueSequence<'a>>},
            quote! {|seq| crate::sqvalue::SqBValueSequence::from_sq_value_sequence(seq, call_info)},
        );

        quote! {
            impl crate::sqvalue::SqValue for #type_path {

                fn get_type_name(&self) -> &'static str {
                    #type_name
                }

                fn get_primitive(
                    &self,
                    call_info: &crate::fieldcall::FieldCallInfo,
                    ) -> crate::error::Result<crate::primitive::Primitive> {
                    self.to_primitive().map_err(|e|
                                                Box::new(crate::error::Error::ToPrimitive {
                                                    span: call_info.ast().ident.span,
                                                    type_name: call_info.type_name().to_owned(),
                                                    source: e,
                                                }
                                                ))
                }

                #get_single_impl
                #get_option_impl
                #get_sequence_impl
            }
        }
    }

    /// Generate the implementation of a `get_` method of `SqValue`.
    fn generate_get_impl(
        type_ext: &TypeExt,
        name: TokenStream2,
        return_sequence_type: ReturnSequenceType,
        return_type: TokenStream2,
        value_mapper: TokenStream2,
    ) -> TokenStream2 {
        let dispatch_arms: Vec<TokenStream2> = type_ext
            .fields
            .iter()
            .filter(|fe| fe.return_sequence_type == return_sequence_type)
            .map(Self::generate_dispatch_arm)
            .collect();

        let inner_impl = if dispatch_arms.is_empty() {
            quote! { panic!("Invalid field"); }
        } else {
            quote! {
                match call_info.field_name() {
                    #(#dispatch_arms.map(#value_mapper),)*
                    _ => panic!("Invalid field"),
                }.map_err(|e| call_info.field_call_error(e))
            }
        };

        // If there are no dispatch arms then the call_info param won't be used, so prefix its name
        // with an underscore.
        let param_name = if dispatch_arms.is_empty() {
            quote! {_call_info}
        } else {
            quote! {call_info}
        };

        quote! {
            fn #name<'a>(&'a self, #param_name: &'a crate::fieldcall::FieldCallInfo) -> #return_type {
                #inner_impl
            }
        }
    }

    fn generate_dispatch_arm(field_ext: &FieldExt) -> TokenStream2 {
        let name = &field_ext.name;
        let method_name = format_ident!("{}", &field_ext.name);

        let args: Vec<TokenStream2> = field_ext
            .params
            .iter()
            .map(|pe| {
                let arg_index = pe.index;
                let arg_name = &pe.name;
                let arg_getter = if pe.required {
                    quote! {arg}
                } else {
                    quote! {opt_arg}
                };
                let (arg_type, copied) = match pe.ty.as_str() {
                    // We're going to get a reference to the arg value as one of the following types.
                    // Unless it's a &str we'll want to clone it before dispatching it to the
                    // field-specific getter.
                    "PrimitiveBool" => (quote! {bool}, quote! {.copied()}),
                    "PrimitiveInt" => (quote! {i64}, quote! {.copied()}),
                    "PrimitiveFloat" => (quote! {f64}, quote! {.copied()}),
                    "PrimitiveString" => (quote! {str}, quote! {}),
                    t => panic!("Unrecognized primitive type {}", t),
                };

                quote! {
                    call_info.#arg_getter::<#arg_type>(#arg_index, #arg_name)? #copied
                }
            })
            .collect();

        quote! {#name => self.#method_name(#(#args),*)}
    }
}

/// Get the external (JSON) schema, deserialized into an `ExtSchema` struct.
fn get_external_schema() -> Result<ExtSchema> {
    let file = File::open("src/system/schema.json")?;
    let reader = BufReader::new(file);
    let ext_schema: ExtSchema = serde_json::from_reader(reader)?;
    Ok(ext_schema)
}

/// Write a `proc_macro2::TokenStream` to a file in the output directory.
fn write_tokens_to_file(tokens: TokenStream2, filename: &str) -> Result<()> {
    let tree: syn::File = syn::parse2(tokens)?;
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let out_path = Path::new(&out_dir).join(filename);
    fs::write(out_path, prettyplease::unparse(&tree))?;

    Ok(())
}

/// Generate all the code.
fn main() -> Result<()> {
    // The code generation only depends on this script and the JSON schema, so tell Cargo to only
    // rerun the script if this script or the JSON schema changes.
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/schema/schema.json");

    let ext_schema = get_external_schema()?;

    let mut schema_generator = SchemaGenerator::new(&ext_schema);
    let schema = schema_generator.generate()?;
    write_tokens_to_file(schema, "schema.gen.rs")?;

    let mut field_traits_generator = SqTypeTraitGenerator::new(&ext_schema);
    let field_traits = field_traits_generator.generate();
    write_tokens_to_file(field_traits, "field_traits.gen.rs")?;

    let mut field_base_implementor = SqValueImplementor::new(&ext_schema);
    let field_base_impls = field_base_implementor.generate();
    write_tokens_to_file(field_base_impls, "field_base_impls.gen.rs")?;

    Ok(())
}
