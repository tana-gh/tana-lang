use tana_lang_codegen::tana_lang;

tana_lang!("./tests/test_001/src.tana");

#[test]
fn test_tana_src_path() {
    let content = tana_src();
    assert_eq!(content, "test\n");
}
