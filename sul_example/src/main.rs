use hyper::{service::make_service_fn, Body, Request, Response, StatusCode};

use std::{
    convert::Infallible,
    net::{Ipv4Addr, SocketAddr, SocketAddrV4},
    sync::Arc,
};

use crate::api::{ApiController, ApiService, Context};

mod api;

#[tokio::main]
async fn main() {
    run().await;
}

async fn run() {
    let addr = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::new(127, 0, 0, 1), 8080));
    let server_builder = hyper::Server::try_bind(&addr).unwrap();

    let users = vec![
        "Olivia".to_owned(),
        "Steven".to_owned(),
        "Robert".to_owned(),
    ];
    let users = Arc::new(users);
    let context = Context { users };

    let server = server_builder.serve(make_service_fn(|_| {
        let context = context.clone();

        async {
            Ok::<_, Infallible>(ApiService::new(
                move || ApiController::new(context.clone()),
                move |request: Request<Body>| {
                    let text = format!("{} {} not found", request.method(), request.uri().path());

                    async {
                        Response::builder()
                            .status(StatusCode::METHOD_NOT_ALLOWED)
                            .body(Body::from(text))
                            .unwrap()
                    }
                },
            ))
        }
    }));

    server.await.unwrap();
}
