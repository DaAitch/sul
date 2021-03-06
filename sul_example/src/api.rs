use std::{
    sync::Arc,
    time::{Duration, SystemTime},
};

#[derive(Clone)]
pub struct Context {
    pub users: Arc<Vec<String>>,
}

pub use service::ApiService;

#[sul::openapi("openapi.yaml")]
pub struct ApiController {
    context: Context,
}

impl ApiController {
    pub fn new(context: Context) -> ApiController {
        ApiController { context }
    }

    pub async fn get_users_id(self, request: GetUsersIdRequest) -> GetUsersIdResponse {
        GetUsersIdResponse::ok(format!("parameter is {}", request.parameters.id)).await
    }

    pub async fn get_users(self, _: GetUsersRequest) -> GetUsersResponse {
        let d = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or(Duration::ZERO);

        if d.as_secs() % 2 == 0 {
            let u: &UserGet = &self.context.users;
            GetUsersResponse::ok(u.clone()).await
        } else {
            GetUsersResponse::unauthorized(GetUsersUnauthorized {
                recoverable: false,
                error_code: "AUTH123".to_owned(),
                error_message: "auth error".to_owned(),
            })
            .await
        }
    }

    pub async fn replace_user(self, request: ReplaceUserRequest) -> ReplaceUserResponse {
        let name = request.body.name;

        ReplaceUserResponse::ok(ReplaceUserOk {
            new_revision: format!("4321_for_{}", name),
            old_revision: "1234".to_owned(),
        })
        .await
    }

    pub async fn get_profiles(self, _: GetProfilesRequest) -> GetProfilesResponse {
        GetProfilesResponse::ok(vec![GetProfilesOkItem {
            id: "1".to_owned(),
            user: GetProfilesOkItemUser {
                first_name: "Robert".to_owned(),
                last_name: "Rust".to_owned(),
            },
        }])
        .await
    }

    pub async fn add_user(self, _: AddUserRequest) -> AddUserResponse {
        AddUserResponse::ok(AddUserOk { id: 4 }).await
    }
}
