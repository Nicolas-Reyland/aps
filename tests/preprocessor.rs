// Tests for preprocessor

#[cfg(test)]
use aps::preprocessor::remove_comments;

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
