/// snake-case
pub fn snake_case(expr: impl AsRef<str>) -> String {
    enum Context {
        PrevStart,
        PrevUpper,
        PrevLower,
        PrevOther,
    }

    let expr = expr.as_ref();
    let mut s = String::with_capacity(2 * expr.len());

    let mut context = Context::PrevStart;

    for ch in expr.chars() {
        match (&context, ch) {
            (Context::PrevUpper | Context::PrevStart, 'A'..='Z') => {
                s.push(ch.to_ascii_lowercase());
                context = Context::PrevUpper;
            }
            (Context::PrevLower | Context::PrevOther, 'A'..='Z') => {
                s.push('_');
                s.push(ch.to_ascii_lowercase());
                context = Context::PrevUpper;
            }
            (Context::PrevStart | Context::PrevLower | Context::PrevUpper, 'a'..='z') => {
                s.push(ch);
                context = Context::PrevLower;
            }
            (Context::PrevOther, 'a'..='z') => {
                s.push('_');
                s.push(ch);
                context = Context::PrevLower;
            }
            (Context::PrevStart, _) => {
                // do nothing
            }
            _ => {
                context = Context::PrevOther;
            }
        }
    }

    s
}

/// upper camel-case
pub fn upper_camel_case(expr: impl AsRef<str>) -> String {
    enum Context {
        MakeUpper,
        MakeLower,
        Any,
    }

    let expr = expr.as_ref();
    let mut s = String::with_capacity(2 * expr.len());

    let mut context = Context::MakeUpper;

    for ch in expr.chars() {
        match (&context, ch) {
            (Context::MakeLower, 'A'..='Z') => {
                s.push(ch.to_ascii_lowercase());
            }
            (Context::MakeUpper | Context::Any, 'A'..='Z') => {
                s.push(ch);
                context = Context::MakeLower;
            }
            (Context::MakeLower | Context::Any, 'a'..='z') => {
                s.push(ch);
                context = Context::Any;
            }
            (Context::MakeUpper, 'a'..='z') => {
                s.push(ch.to_ascii_uppercase());
                context = Context::MakeLower;
            }
            _ => {
                context = Context::MakeUpper;
            }
        }
    }

    s
}

// tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn some_tests() {
        assert_eq!(snake_case("errorCode"), "error_code".to_owned());
        assert_eq!(snake_case("/users"), "users".to_owned());
        assert_eq!(snake_case("/some/thing"), "some_thing".to_owned());
        assert_eq!(upper_camel_case("/users"), "Users".to_owned());

        assert_eq!(snake_case("Some Value"), "some_value".to_owned());
        assert_eq!(upper_camel_case("Some Value"), "SomeValue".to_owned());

        assert_eq!(snake_case("OK"), "ok".to_owned());
        assert_eq!(
            upper_camel_case("OK this is Okay, REALLY"),
            "OkThisIsOkayReally".to_owned()
        );
        assert_eq!(
            snake_case("OK this is Okay, REALLY"),
            "ok_this_is_okay_really".to_owned()
        );

        assert_eq!(upper_camel_case("getUser"), "GetUser".to_owned());
    }
}
