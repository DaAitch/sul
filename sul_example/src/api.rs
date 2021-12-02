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

    pub async fn get_users_id_(self, request: GetUsersIdRequest) -> GetUsersIdResponse {
        GetUsersIdResponse::ok(&format!("parameter is {}", request.id))
    }

    pub async fn get_users(self, _: GetUsersRequest) -> GetUsersResponse {
        let d = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or(Duration::ZERO);

        if d.as_secs() % 2 == 0 {
            GetUsersResponse::ok(&self.context.users)
        } else {
            GetUsersResponse::unauthorized(&GetUsersUnauthorized {
                error_code: "AUTH123".to_owned(),
                error_message: "auth error".to_owned(),
            })
        }
    }

    pub async fn put_users_id_(self, request: PutUsersIdRequest) -> PutUsersIdResponse {
        PutUsersIdResponse::ok(&format!("okay {} replaced", request.id))
    }

    pub async fn get_profiles(self, _: GetProfilesRequest) -> GetProfilesResponse {
        GetProfilesResponse::ok(&vec![GetProfilesOk {
            id: "1".to_owned(),
            user: GetProfilesOkUser {
                first_name: "Robert".to_owned(),
                last_name: "Rust".to_owned(),
            },
        }])
    }
}
