// Preprocessor

use std::fs;

pub fn read_and_preprocess_file(filename: &str) -> String {
    let mut content = fs::read_to_string(filename).unwrap();
    // remove comments
    content = remove_comments(&content);
    // run macros
    run_macros(&content)
}

pub fn remove_comments(src: &str) -> String {
    let mut src_chars = src.chars();
    let src_len = src.len();
    let mut new_src = String::with_capacity(src_len);
    if src_len <= 2 {
        return src.to_string();
    }
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

fn run_macros(src: &str) -> String {
    /* e.g.
        #use numbers.aspl
        #
     */
    src.to_string() // TODO
}
