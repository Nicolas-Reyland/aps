
use crate::parser;
use crate::preprocessor::read_and_preprocess_file;

pub fn exec_macro_use(macro_str: &str, imports: Option<Vec<&str>>) -> String {
    let mut content = String::new();
    let filenames_str = macro_str[4..].to_string();
    let filenames: Vec<&str> = filenames_str.split(' ').collect();
    for filename in filenames {
        if filename.is_empty() {
            continue;
        }
        // test for import-cycle
        if imports.is_some() && imports.as_ref().unwrap().contains(&filename) {
            panic!(
                "Import cycle detected for {} : {:?}",
                filename,
                imports.unwrap()
            )
        }
        match read_and_preprocess_file(
            filename,
            match imports.clone() {
                Some(mut imports) => {
                    imports.push(&filename);
                    Some(imports)
                }
                None => Some(vec![&filename]),
            },
        ) {
            Some(s) => content.push_str(&s),
            None => {
                eprintln!(" import for '{}' was unsuccessful (#use)", filename);
                continue;
            }
        }
        // to separate file contents (which might not even end with a new-line char)
        content.push('\n');
    }
    return content;
}

pub fn exec_macro_add(macro_str: &str) -> String {
    // #add + associative,commutative
    let mut content = String::new();
    let (op_string, rest) = extract_operator_rest(macro_str, 5);
    let op_str = op_string.as_str();
    let attr_str = rest.to_string().replace("\\s", "");
    for attr in attr_str.split(",") {
        match attr {
            "a" | "ass" | "associative" => {
                content.push_str("_ :: { (A @ B) @ C = A @ (B @ C) ; }\n".replace("@", op_str).as_str())
            }
            "c" | "com" | "commutative" => {
                content.push_str("_ :: { A @ B = B @ A ; }\n".replace("@", op_str).as_str())
            }
            _ => panic!("invalid attribute: <{}>", attr),
        }
    }

    content
}

pub fn exec_macro_identity(macro_str: &str) -> String {
    // #identity + 0
    // #identity * 1
    let (op_string, value_str) = extract_operator_rest(macro_str, 10);
    let value = value_str.parse::<i32>().expect(format!("Failed to parse <{}> as a number", value_str).as_str());
    format!("_ :: {{ A {} {} = A ; }}\n", op_string, value)
}

pub fn exec_macro_null(macro_str: &str) -> String {
    // #null * 0
    let (op_string, value_str) = extract_operator_rest(macro_str, 6);
    let value = value_str.parse::<i32>().expect(format!("Failed to parse <{}> as a number", value_str).as_str());
    format!("_ :: {{ A {} {} = {} ; }}\n", op_string, value, value)
}

fn extract_operator_rest(content: &str, op_index: usize) -> (String, String) {
    let op_str = content[op_index..op_index+1].to_string();
    let op_string = parser::op_p::<parser::ApsParserKind>(&op_str)
        .expect(&format!("Failed to parse operator: <{}> in <{}>", op_str, content))
        .1.op.to_string();
    (op_string, content[op_index + 2..].to_string())
}
