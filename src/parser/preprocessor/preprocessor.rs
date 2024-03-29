// Preprocessor

use std::fs;

mod macros;

pub fn read_and_preprocess_file(filename: &str, imports: Option<Vec<&str>>) -> Option<String> {
    let mut content = match fs::read_to_string(filename) {
        Ok(s) => s,
        _ => return None,
    };
    // remove comments
    content = remove_comments(&content);
    // run macros
    Some(process_macros(&content, imports))
}

pub fn remove_comments(src: &str) -> String {
    let src_len = src.len();
    if src_len < 2 {
        return src.to_string();
    }
    let mut new_src = String::with_capacity(src_len);
    let mut src_chars = src.chars();
    let mut in_multi_line_comment = false;
    let mut in_single_line_comment = false;
    let mut skip_next = false;
    // consume the two first characters
    let mut c1 = src_chars.next().unwrap();
    let mut c2 = src_chars.next().unwrap();
    loop {
        // inside single-line comments
        if in_single_line_comment {
            if c2 == '\n' {
                in_single_line_comment = false;
            }
        }
        // inside multi-line comments
        else if in_multi_line_comment {
            if (c1, c2) == ('*', '/') {
                in_multi_line_comment = false;
                skip_next = true;
            }
        }
        // normal code
        else {
            if skip_next {
                skip_next = false;
            } else {
                // detect comments
                match (c1, c2) {
                    ('/', '/') => {
                        in_single_line_comment = true;
                    }
                    ('/', '*') => {
                        in_multi_line_comment = true;
                    }
                    _ => new_src.push(c1),
                }
            }
        }
        c1 = c2;
        c2 = match src_chars.next() {
            Some(c) => c,
            None => break,
        };
    }
    if !(in_single_line_comment || in_multi_line_comment) {
        new_src.push(c1);
    }
    new_src
}

pub fn process_macros(src: &str, imports: Option<Vec<&str>>) -> String {
    /* e.g.
       #use numbers.apsl
       #
    */
    let src_len = src.len();
    if src_len < 2 {
        return src.to_string();
    }
    let mut src_chars = src.chars();
    let mut new_src = String::with_capacity(src_len);
    // consume the two first characters
    let mut c1 = src_chars.next().unwrap();
    let mut c2 = src_chars.next().unwrap();
    // src starts with a macro instruction
    if c1 == '#' {
        let mut macro_str = String::new();
        macro_str.push(c1);
        let mut next_c = c2;
        while next_c != '\n' {
            macro_str.push(next_c);
            next_c = match src_chars.next() {
                Some(c) => c,
                // content of file is only a macro instruction (not even new-line char)
                None => return expand_macro(&macro_str, imports),
            }
        }
        c1 = next_c; // next_c == '\n'
        new_src = expand_macro(&macro_str, imports.clone());
        c2 = match src_chars.next() {
            Some(c) => c,
            None => {
                new_src.push(c1);
                return new_src;
            }
        }
    }
    // look for other macro instructions
    loop {
        if (c1, c2) == ('\n', '#') {
            new_src.push(c1); // '\n'
            let mut macro_str = String::new();
            let mut next_c = c2;
            while next_c != '\n' {
                macro_str.push(next_c);
                next_c = match src_chars.next() {
                    Some(c) => c,
                    None => {
                        new_src.push_str(&expand_macro(&macro_str, imports.clone()));
                        // file ends with a macro instruction, without a trailing '\n'
                        return new_src;
                    }
                }
            }
            new_src.push_str(&expand_macro(&macro_str, imports.clone()));
            c1 = next_c; // next_c == '\n'
            c2 = match src_chars.next() {
                Some(c) => c,
                None => break,
            };
            continue;
        }
        new_src.push(c1);
        c1 = c2;
        c2 = match src_chars.next() {
            Some(c) => c,
            None => break,
        }
    }
    new_src.push(c1);
    new_src
}

fn expand_macro(macro_str: &str, imports: Option<Vec<&str>>) -> String {
    if macro_str.starts_with("#use") {
        return macros::exec_macro_use(macro_str, imports);
    }
    if macro_str.starts_with("#add") {
        return macros::exec_macro_add(macro_str);
    }
    if macro_str.starts_with("#identity") {
        return macros::exec_macro_identity(macro_str);
    }
    if macro_str.starts_with("#null") {
        return macros::exec_macro_null(macro_str);
    }
    panic!("Unknown macro instruction: '{}'", macro_str);
}
