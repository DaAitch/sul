//! <https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md>

use serde::{
    de::{MapAccess, Visitor},
    Deserialize, Deserializer,
};
use std::{collections::HashMap, fmt, path::Path};

#[derive(Debug)]
pub enum Error {
    ReadFile(std::io::Error),
    Yaml(serde_yaml::Error),
}

type Result<T> = std::result::Result<T, Error>;

pub fn read_openapi<'a>(file_path: impl AsRef<Path>) -> Result<Document> {
    let file = std::fs::File::open(file_path).map_err(Error::ReadFile)?;
    let spec: Document = serde_yaml::from_reader(file).map_err(Error::Yaml)?;
    Ok(spec)
}

#[derive(Deserialize, Debug)]
pub struct Document {
    pub openapi: String,
    pub info: InfoObject,
    pub servers: Vec<ServerObject>,
    pub paths: HashMap<String, PathItemObject>,
}

#[derive(Deserialize, Debug)]
pub struct InfoObject {
    pub title: Option<String>,
    pub description: Option<String>,
    pub version: Option<String>,
}

#[derive(Deserialize, Debug)]
pub struct ServerObject {
    pub url: String,
    pub description: Option<String>,
}

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

#[derive(Deserialize, Debug)]
pub struct OperationObject {
    #[serde(rename = "operationId")]
    pub operation_id: Option<String>,
    pub summary: Option<String>,
    pub description: Option<String>,
    pub responses: HashMap<String, ResponseObject>, // response or ref-object
    pub parameters: Option<Vec<ParameterObject>>,
}

/// https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#responseObject
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

#[derive(Deserialize, Debug)]
pub struct MediaTypeObjectJson {
    pub schema: SchemaObject,
}

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
                #[serde(field_identifier, rename_all = "lowercase")]
                enum Schema {
                    Type,
                    Items,
                    Properties,
                }

                #[derive(Deserialize, Debug)]
                #[serde(field_identifier, rename_all = "lowercase")]
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

#[derive(Deserialize, Debug)]
pub struct ParameterObject {
    pub name: String,
    pub r#in: ParameterLocation,
    pub description: Option<String>,
    pub required: bool,
    pub deprecated: bool,
}

#[derive(Deserialize, Debug)]
pub enum ParameterLocation {
    #[serde(rename = "query")]
    Query,
    #[serde(rename = "header")]
    Header,
    #[serde(rename = "path")]
    Path,
    #[serde(rename = "cookie")]
    Cookie,
}
