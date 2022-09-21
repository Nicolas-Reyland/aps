// Preprocessor

use std::fs;

pub fn read_and_preprocess_file(filename: &str) -> String {
    let mut content = fs::read_to_string(filename).unwrap();
    // remove comments
    content = remove_comments(&content);
    // run macros
    process_macros(&content)
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
                    },
                    ('/', '*') => {
                        in_multi_line_comment = true;
                    },
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
    if ! (in_single_line_comment || in_multi_line_comment) {
        new_src.push(c1);
    }
    new_src
}

pub fn process_macros(src: &str) -> String {
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
                None => return expand_macro(&macro_str),
            }
        }
        c1 = next_c; // next_c == '\n'
        new_src = expand_macro(&macro_str);
        c2 = match src_chars.next() {
            Some(c) => c,
            None => {
                new_src.push(c1);
                return new_src
            },
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
                        new_src.push_str(&expand_macro(&macro_str));
                        // file ends with a macro instruction, without a trailing '\n'
                        return new_src;
                    },
                }
            }
            new_src.push_str(&expand_macro(&macro_str));
            c1 = next_c; // next_c == '\n'
            c2 = match src_chars.next() {
                Some(c) => c,
                None => break,
            }
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

fn expand_macro(macro_str: &str) -> String {
    if macro_str.starts_with("#use ") {
        let mut content = String::new();
        let filenames_str = macro_str[4..].to_string();
        let filenames: Vec<&str> = filenames_str.split(' ').collect();
        for filename in filenames {
            if filename.is_empty() {
                continue;
            }
            content.push_str(&read_and_preprocess_file(filename));
            // to separate file contents (which might not even end with a new-line char)
            content.push('\n');
        }
        return content;
    }
    panic!("Unknown macro instruction: '{}'", macro_str);
}
