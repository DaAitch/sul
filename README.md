# Sul - An WIP OpenAPI Rust Tower Service generator

Generating Rust code for a Contract-First OpenAPI approach.

The example project is an already running example and this is just where it starts.

## Example

Given an OpenAPI spec:

```yaml
openapi: 3.0.0
# omitted
paths:
  /users:
    get:
      summary: Returns all users.
      description: Some text
      responses:
        '200':
          description: Some text
          content:
            application/json:
              schema:
                type: array
                items: 
                  type: string
        '401':
          description: Some text
          content:
            application/json:
              schema:
                type: object
                properties:
                  errorCode:
                    type: string
                  errorMessage:
                    type: string                  
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

    pub async fn get_users(self, _: GetUsersRequest) -> GetUsersResponse {
        todo!()
    }
}
```

so that I can concentrate on my real problems.
