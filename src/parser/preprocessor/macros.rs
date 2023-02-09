
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
    let op_str = macro_str[5..6].to_string();
    let op_string = parser::op_p::<parser::ApsParserKind>(&op_str)
        .expect(&format!("Failed to parse operator: <{}> in <{}>", op_str, macro_str))
        .1.op.to_string();
    let op_str = op_string.as_str();
    let attr_str = macro_str[7..].to_string().replace("\\s", "");
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
