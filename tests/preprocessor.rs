// Tests for preprocessor

#[cfg(test)]
use aps::preprocessor::{process_macros, remove_comments};

#[test]
fn test_remove_comments() {
    let src = "/* start comment */
this is code // and this is not */
I /**/just/**/ want to // test things
// break things
do stuff
//
";
    assert_eq!(
        remove_comments(src),
        "\n".to_string() + "this is code \n" + "I just want to \n" + "\n" + "do stuff\n" + "\n"
    );
}

#[test]
fn block_parent_dir_use_macro() {
    let src = "#use ../etc/passwd\n";
    let expanded = process_macros(src, None);
    assert_eq!(expanded.trim(), "");
}
