//! <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md>

use hyper::Method;
use serde::{
    de::{MapAccess, Visitor},
    Deserialize, Deserializer,
};
use std::{collections::HashMap, fmt, path::Path};

#[derive(Debug)]
pub enum ReadFileError {
    ReadFile(std::io::Error),
    Yaml(serde_yaml::Error),
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#openapi-object>
#[derive(Deserialize, Debug)]
pub struct Document {
    pub openapi: String,
    pub info: InfoObject,
    pub servers: Vec<ServerObject>,
    // TODO(daaitch): make optional
    pub paths: HashMap<String, PathItemObjectOrRef>,
    pub components: Option<ComponentsObject>,
}

impl Document {
    pub fn from_file(file_path: impl AsRef<Path>) -> Result<Self, ReadFileError> {
        let file = std::fs::File::open(file_path).map_err(ReadFileError::ReadFile)?;
        let spec: Document = serde_yaml::from_reader(file).map_err(ReadFileError::Yaml)?;
        Ok(spec)
    }

    pub fn get_path_item_ref<'a>(
        &'a self,
        object_or_ref: &'a PathItemObjectOrRef,
    ) -> Result<&'a PathItemObject, ()> {
        match object_or_ref {
            PathItemObjectOrRef::Object(object) => Ok(object),
            PathItemObjectOrRef::Ref(r) => {
                let components = self.components.as_ref().ok_or(())?;
                let path_items = components.path_items.as_ref().ok_or(())?;
                let path_item = path_items.get(&r.name).ok_or(())?;

                self.get_path_item_ref(path_item) // recursion
            }
        }
    }
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#info-object>
#[derive(Deserialize, Debug)]
pub struct InfoObject {
    pub title: Option<String>,
    pub description: Option<String>,
    pub version: Option<String>,
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#server-object>
#[derive(Deserialize, Debug)]
pub struct ServerObject {
    pub url: String,
    pub description: Option<String>,
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#path-item-object>
#[derive(Deserialize, Debug)]
pub struct PathItemObject {
    pub get: Option<OperationObject>,
    pub put: Option<OperationObject>,
    pub post: Option<OperationObject>,
    pub delete: Option<OperationObject>,
    pub options: Option<OperationObject>,
    pub head: Option<OperationObject>,
    pub patch: Option<OperationObject>,
    pub trace: Option<OperationObject>,
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#path-item-object>
#[derive(Debug)]
pub struct PathItemObjectRef {
    name: String,
}

// TODO(daaitch): duplicated code, see SchemaObjectRef
impl<'de> Deserialize<'de> for PathItemObjectRef {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        struct V;

        const PREFIX: &str = "#/components/pathItems/";

        impl<'de> Visitor<'de> for V {
            type Value = PathItemObjectRef;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "{}..", PREFIX)
            }

            fn visit_str<E: serde::de::Error>(
                self,
                v: &str,
            ) -> std::result::Result<Self::Value, E> {
                if v.starts_with(PREFIX) {
                    Ok(PathItemObjectRef {
                        name: v[PREFIX.len()..].to_string(),
                    })
                } else {
                    Err(serde::de::Error::invalid_value(
                        serde::de::Unexpected::Str(v),
                        &self,
                    ))
                }
            }
        }

        deserializer.deserialize_str(V)
    }
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#path-item-object>
#[derive(Debug)]
pub enum PathItemObjectOrRef {
    Ref(PathItemObjectRef),
    Object(PathItemObject),
}

// TODO(daaitch): duplicated code, see SchemaObjectOrRef
impl<'de> Deserialize<'de> for PathItemObjectOrRef {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = PathItemObjectOrRef;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Path Item Object or $ref")
            }

            fn visit_map<A: MapAccess<'de>>(
                self,
                mut map: A,
            ) -> std::result::Result<Self::Value, A::Error> {
                #[derive(Deserialize, Debug, PartialEq, Eq, Hash)]
                #[serde(field_identifier, rename_all = "camelCase")]
                enum Key {
                    #[serde(rename = "$ref")]
                    Ref,
                    Get,
                    Put,
                    Post,
                    Delete,
                    Options,
                    Head,
                    Patch,
                    Trace,
                }

                let mut r#ref: Option<PathItemObjectRef> = None;
                let mut methods: HashMap<Key, OperationObject> = Default::default();

                while let Some(key) = map.next_key::<Key>()? {
                    match key {
                        Key::Ref => {
                            r#ref = Some(map.next_value()?);
                        }
                        method => {
                            methods.insert(method, map.next_value()?);
                        }
                    }
                }

                match r#ref {
                    Some(r#ref) => {
                        if !methods.is_empty() {
                            return Err(serde::de::Error::invalid_value(
                                serde::de::Unexpected::StructVariant,
                                &self,
                            ));
                        }

                        Ok(PathItemObjectOrRef::Ref(r#ref))
                    }
                    None => Ok(PathItemObjectOrRef::Object(PathItemObject {
                        get: methods.remove(&Key::Get),
                        put: methods.remove(&Key::Put),
                        post: methods.remove(&Key::Post),
                        delete: methods.remove(&Key::Delete),
                        options: methods.remove(&Key::Options),
                        head: methods.remove(&Key::Head),
                        patch: methods.remove(&Key::Patch),
                        trace: methods.remove(&Key::Trace),
                    })),
                }
            }
        }

        deserializer.deserialize_any(V)
    }
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#operation-object>
#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct OperationObject {
    pub operation_id: Option<String>,
    pub summary: Option<String>,
    pub description: Option<String>,
    pub responses: HashMap<String, ResponseObject>, // response or ref-object
    pub parameters: Option<Vec<ParameterObject>>,
    pub request_body: Option<RequestBodyObject>,
}

impl OperationObject {
    pub fn check_path_parameters<'a>(
        &self,
        parameters: impl IntoIterator<Item = &'a String>,
        method: &Method,
        path: impl AsRef<str>,
    ) -> std::result::Result<(), String> {
        for parameter in parameters {
            let what = || format!("'{}' in '{} {}'", parameter, method, path.as_ref());

            let parameters = self
                .parameters
                .as_ref()
                .ok_or_else(|| format!("absent parameters for {}", what()))?;

            match parameters
                .into_iter()
                .find(|p| &p.name == parameter && p.r#in == ParameterLocation::Path)
            {
                Some(param) => {
                    if param.required != Some(true) {
                        Err(format!(
                            "parameter {} is missing field 'required = true'",
                            what()
                        ))
                    } else {
                        Ok(())
                    }
                }
                None => Err(format!("missing path parameter for {}", what())),
            }?;
        }

        Ok(())
    }
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#responseObject>
#[derive(Deserialize, Debug)]
pub struct ResponseObject {
    pub description: String,
    pub content: MediaTypeObjectMap,
}

#[derive(Deserialize, Debug)]
pub struct MediaTypeObjectMap {
    #[serde(rename = "application/json")]
    pub application_json: MediaTypeObjectJson,
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#media-type-object>
#[derive(Deserialize, Debug)]
pub struct MediaTypeObjectJson {
    pub schema: SchemaObjectOrRef,
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#schema-object>
#[derive(Debug)]
pub enum SchemaObject {
    String(DataTypeStringFormat),
    Integer(DataTypeIntegerFormat),
    Number(DataTypeNumberFormat),
    Array(Box<SchemaObjectOrRef>),
    Object(Box<HashMap<String, SchemaObjectOrRef>>),
}

#[derive(Debug)]
pub enum DataTypeStringFormat {
    None,
    Password,
}

#[derive(Debug)]
pub enum DataTypeIntegerFormat {
    Int32,
    Int64,
}

#[derive(Debug)]
pub enum DataTypeNumberFormat {
    Float,
    Double,
}

impl<'de> Deserialize<'de> for SchemaObject {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        let object_or_ref = SchemaObjectOrRef::deserialize(deserializer)?;

        match object_or_ref {
            SchemaObjectOrRef::Object(object) => Ok(object),
            SchemaObjectOrRef::Ref(_) => Err(serde::de::Error::custom(
                "unexpected $ref, expecting Schema Object",
            )),
        }
    }
}

#[derive(Debug)]
pub struct SchemaObjectRef {
    pub name: String,
}

// TODO(daaitch): duplicated code, see PathItemObjectRef
impl<'de> Deserialize<'de> for SchemaObjectRef {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct V;

        const PREFIX: &str = "#/components/schemas/";

        impl<'de> Visitor<'de> for V {
            type Value = SchemaObjectRef;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "{}..", PREFIX)
            }

            fn visit_str<E: serde::de::Error>(
                self,
                v: &str,
            ) -> std::result::Result<Self::Value, E> {
                if v.starts_with(PREFIX) {
                    Ok(SchemaObjectRef {
                        name: v[PREFIX.len()..].to_string(),
                    })
                } else {
                    Err(serde::de::Error::invalid_value(
                        serde::de::Unexpected::Str(v),
                        &self,
                    ))
                }
            }
        }

        deserializer.deserialize_str(V)
    }
}

#[derive(Debug)]
pub enum SchemaObjectOrRef {
    Ref(SchemaObjectRef),
    Object(SchemaObject),
}

// TODO(daaitch): duplicated code, see PathItemObjectOrRef
impl<'de> Deserialize<'de> for SchemaObjectOrRef {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = SchemaObjectOrRef;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "Schema Object or $ref")
            }

            fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
                #[derive(Deserialize, Debug)]
                #[serde(field_identifier, rename_all = "camelCase")]
                enum Schema {
                    #[serde(rename = "$ref")]
                    Ref,
                    Type,
                    Items,
                    Properties,
                    Format,
                }

                #[derive(Deserialize, Debug)]
                #[serde(field_identifier, rename_all = "camelCase")]
                enum Type {
                    String,
                    Integer,
                    Number,
                    Array,
                    Object,
                }

                #[derive(Deserialize, Debug)]
                #[serde(field_identifier, rename_all = "camelCase")]
                enum DataTypeFormat {
                    #[serde(skip)]
                    None,
                    Password,
                    Int32,
                    Int64,
                    Float,
                    Double,
                }

                let mut r: Option<SchemaObjectRef> = None;
                let mut ty: Option<Type> = None;
                let mut items: Option<SchemaObjectOrRef> = None;
                let mut properties: Option<HashMap<String, SchemaObjectOrRef>> = None;
                let mut format = DataTypeFormat::None;

                while let Some(key) = map.next_key::<Schema>()? {
                    match key {
                        Schema::Type => {
                            ty = Some(map.next_value()?);
                        }
                        Schema::Items => {
                            items = Some(map.next_value()?);
                        }
                        Schema::Properties => {
                            properties = Some(map.next_value()?);
                        }
                        Schema::Ref => {
                            r = Some(map.next_value()?);
                        }
                        Schema::Format => {
                            format = map.next_value()?;
                        }
                    }
                }

                match r {
                    Some(r) => match (ty, items, properties) {
                        (None, None, None) => Ok(SchemaObjectOrRef::Ref(r)),
                        _ => Err(serde::de::Error::custom(
                            "given $ref, doesn't expecting other fields",
                        )),
                    },
                    None => match ty {
                        None => Err(serde::de::Error::missing_field("type")),
                        Some(Type::String) => match format {
                            DataTypeFormat::None => Ok(SchemaObjectOrRef::Object(
                                SchemaObject::String(DataTypeStringFormat::None),
                            )),
                            DataTypeFormat::Password => Ok(SchemaObjectOrRef::Object(
                                SchemaObject::String(DataTypeStringFormat::Password),
                            )),
                            invalid_format => Err(serde::de::Error::custom(format!(
                                "unexpected format = {:?} for type string",
                                invalid_format
                            ))),
                        },
                        Some(Type::Integer) => match format {
                            DataTypeFormat::Int32 => Ok(SchemaObjectOrRef::Object(
                                SchemaObject::Integer(DataTypeIntegerFormat::Int32),
                            )),
                            DataTypeFormat::Int64 => Ok(SchemaObjectOrRef::Object(
                                SchemaObject::Integer(DataTypeIntegerFormat::Int64),
                            )),
                            invalid_format => Err(serde::de::Error::custom(format!(
                                "unexpected format = {:?} for type integer",
                                invalid_format
                            ))),
                        },
                        Some(Type::Number) => match format {
                            DataTypeFormat::Float => Ok(SchemaObjectOrRef::Object(
                                SchemaObject::Number(DataTypeNumberFormat::Float),
                            )),
                            DataTypeFormat::Double => Ok(SchemaObjectOrRef::Object(
                                SchemaObject::Number(DataTypeNumberFormat::Double),
                            )),
                            invalid_format => Err(serde::de::Error::custom(format!(
                                "unexpected format = {:?} for type integer",
                                invalid_format
                            ))),
                        },
                        Some(Type::Array) => match items {
                            None => Err(serde::de::Error::missing_field("items")),
                            Some(items) => Ok(SchemaObjectOrRef::Object(SchemaObject::Array(
                                Box::new(items),
                            ))),
                        },
                        Some(Type::Object) => match properties {
                            None => Err(serde::de::Error::missing_field("properties")),
                            Some(properties) => Ok(SchemaObjectOrRef::Object(
                                SchemaObject::Object(Box::new(properties)),
                            )),
                        },
                    },
                }
            }
        }

        deserializer.deserialize_map(V)
    }
}

#[derive(Debug)]
pub enum ParameterObjectContentOrSchema {
    Content(MediaTypeObjectMap),
    Schema(SchemaObjectOrRef),
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#parameter-object>
#[derive(Debug)]
pub struct ParameterObject {
    pub name: String,
    pub r#in: ParameterLocation,
    pub description: Option<String>,
    pub required: Option<bool>,
    pub deprecated: Option<bool>,
    pub schema_or_content: ParameterObjectContentOrSchema,
}

impl<'de> Deserialize<'de> for ParameterObject {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize, Debug)]
        struct ParameterObjectReadHelper {
            name: String,
            r#in: ParameterLocation,
            description: Option<String>,
            required: Option<bool>,
            deprecated: Option<bool>,
            schema: Option<SchemaObjectOrRef>,
            content: Option<MediaTypeObjectMap>,
        }

        let object = ParameterObjectReadHelper::deserialize(deserializer)?;

        let schema_or_content = match (object.schema, object.content) {
            (Some(schema), None) => ParameterObjectContentOrSchema::Schema(schema),
            (None, Some(content)) => ParameterObjectContentOrSchema::Content(content),
            _ => {
                return Err(serde::de::Error::custom(
                    "expected either schema or content",
                ))
            }
        };

        Ok(ParameterObject {
            name: object.name,
            r#in: object.r#in,
            description: object.description,
            required: object.required,
            deprecated: object.deprecated,
            schema_or_content,
        })
    }
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#parameter-locations>
#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum ParameterLocation {
    Query,
    Header,
    Path,
    Cookie,
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#request-body-object>
#[derive(Deserialize, Debug)]
pub struct RequestBodyObject {
    pub description: Option<String>,
    pub content: MediaTypeObjectMap,
    pub required: Option<bool>,
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#components-object>
#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ComponentsObject {
    pub schemas: Option<HashMap<String, SchemaObjectOrRef>>,
    pub path_items: Option<HashMap<String, PathItemObjectOrRef>>,
}
