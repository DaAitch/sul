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
        path_item_ref: &PathItemObjectRef,
    ) -> std::result::Result<&'a PathItemObject, ()> {
        let components = self.components.as_ref().ok_or(())?;
        let path_items = components.path_items.as_ref().ok_or(())?;
        let path_item = path_items.get(&path_item_ref.name).ok_or(())?;

        match path_item {
            PathItemObjectOrRef::Object(object) => Ok(object),
            PathItemObjectOrRef::Ref(r) => self.get_path_item_ref(r), // recursion
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

impl PathItemObjectOrRef {
    pub fn get_or_find<'a>(&'a self, document: &'a Document) -> Option<&'a PathItemObject> {
        match &self {
            &PathItemObjectOrRef::Object(object) => Some(object),
            &PathItemObjectOrRef::Ref(r) => {
                if let Ok(p) = document.get_path_item_ref(r) {
                    Some(p)
                } else {
                    None
                }
            }
        }
    }
}

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
    pub schema: SchemaObject,
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#schema-object>
#[derive(Debug)]
pub enum SchemaObject {
    Array(Box<SchemaObject>),
    Object(Box<HashMap<String, SchemaObject>>),
    String,
}

impl<'de> Deserialize<'de> for SchemaObject {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = SchemaObject;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("schema")
            }

            fn visit_map<A: MapAccess<'de>>(
                self,
                mut map: A,
            ) -> std::result::Result<Self::Value, A::Error> {
                #[derive(Deserialize, Debug)]
                #[serde(field_identifier, rename_all = "camelCase")]
                enum Schema {
                    Type,
                    Items,
                    Properties,
                }

                #[derive(Deserialize, Debug)]
                #[serde(field_identifier, rename_all = "camelCase")]
                enum Type {
                    Array,
                    String,
                    Object,
                }

                let mut ty: Option<Type> = None;
                let mut items: Option<SchemaObject> = None;
                let mut properties: Option<HashMap<String, SchemaObject>> = None;

                loop {
                    match map.next_key()? {
                        Some(Schema::Type) => {
                            ty = Some(map.next_value()?);
                        }
                        Some(Schema::Items) => {
                            items = Some(map.next_value()?);
                        }
                        Some(Schema::Properties) => {
                            properties = Some(map.next_value()?);
                        }
                        None => {
                            break;
                        }
                    }
                }

                match ty {
                    None => Err(serde::de::Error::missing_field("type")),
                    Some(Type::Array) => match items {
                        None => Err(serde::de::Error::missing_field("items")),
                        Some(items) => Ok(SchemaObject::Array(Box::new(items))),
                    },
                    Some(Type::Object) => match properties {
                        None => Err(serde::de::Error::missing_field("properties")),
                        Some(properties) => Ok(SchemaObject::Object(Box::new(properties))),
                    },
                    Some(Type::String) => Ok(SchemaObject::String),
                }
            }
        }

        const FIELDS: &[&str] = &["type", "items", "properties"];

        deserializer.deserialize_struct("SpecSchema", FIELDS, V)
    }
}

/// <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#parameter-object>
#[derive(Deserialize, Debug)]
pub struct ParameterObject {
    pub name: String,
    pub r#in: ParameterLocation,
    pub description: Option<String>,
    pub required: Option<bool>,
    pub deprecated: Option<bool>,
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
    // pub schemas: Option<HashMap<String, SchemaObject>>,
    pub path_items: Option<HashMap<String, PathItemObjectOrRef>>,
}
