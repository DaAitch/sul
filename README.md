# sul

[![Rust](https://github.com/DaAitch/sul/actions/workflows/rust.yml/badge.svg)](https://github.com/DaAitch/sul/actions/workflows/rust.yml)

*An WIP OpenAPI Rust Tower Service generator*

<img src="sul-logo.svg" width="400"/> 

Generating Rust code for a Contract-First OpenAPI approach.

The example project is an already running example and this is just where it starts.

## Example

Given an OpenAPI spec:

```yaml
openapi: 3.0.0
# omitted
paths:
  /users/{id}:
    get:
      summary: Returns a user by id.
      description: Some text
      responses:
        '200':
          description: Some text
          content:
            application/json:
              schema:
                type: string
      parameters:
        - name: id
          in: path
          required: true
```

I want to implement an HTTP server according to a contract without writing boilerplate code 

```rust
#[sul::openapi("openapi.yaml")]
pub struct ApiController {
    context: Context,
}

impl ApiController {
    pub fn new(context: Context) -> ApiController {
        ApiController { context }
    }

    pub async fn get_users_id_(self, request: GetUsersIdRequest) -> GetUsersIdResponse {
        GetUsersIdResponse::ok(&format!("parameter is {}", request.id))
    }
}
```

so that I can concentrate on my real problems.

## Features

There are a lot of missing features which are easy to implement, but the current focus in on crosscutting.

- OpenAPI file formats
  - [x] Yaml
  - [ ] JSON
- Paths
  - [x] Path Templates
  - [x] Request Body
  - [x] $ref Path Item Object
- Parameter Types
  - [x] Path
  - [ ] Query
  - [ ] Header
  - [ ] Cookie
- Methods
  - [x] GET
  - [x] PUT
  - [x] POST
  - [x] DELETE
  - [x] OPTIONS
  - [x] HEAD
  - [x] PATCH
  - [x] TRACE
- Responses
  - [x] 200: Ok
  - [x] 401: Unauthorized
  - [ ] other status codes
- Content types
  - [x] JSON
  - [ ] other formats
- Schema Types
  - [x] String
  - [ ] Integer
  - [ ] Number
  - [x] Array
  - [x] Object
  - [ ] other types
  - [x] $ref Schema Object
- Components
  - [x] Path Items
  - [x] Schemas
  - [ ] other maps
- [ ] external $ref files
