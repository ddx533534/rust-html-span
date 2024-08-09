use rust_html_span_lib::parse;

pub  fn main() {
    let html = r#"<span>hello world!I am <b>18</b>!</span>"#;
    let html_info = parse(html);
    println!("{}", html_info);
}