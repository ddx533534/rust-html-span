# rust-html-span

use rust library **[html5ever](https://github.com/servo/html5ever)** and **[cssparser](https://github.com/servo/rust-cssparser)** to transfer html fragment to text spans!

1. base spanInfos ✅
2. support line-break and space ✅
3. support properties parse ✅

for example, you have a html fragment like as following:

```text
<span style="color: red;text-align:right" >hello world! I am <span style="color: blue; font-weight: bold;">18</span></span>
```

transfer result:

```text
[id:2, parent_id:0, tag: Span, range:0..20, properties:[]]
[id:3, parent_id:2, tag: Span, range:0..20, properties:[Property { key: "color", val: "red" }, Property { key: "text-align", val: "right" }]]
[id:5, parent_id:3, tag: Span, range:18..20, properties:[Property { key: "color", val: "blue" }, Property { key: "font-weight", val: "bold" }]]
text: hello world!I am 18!
```

usage:

```rust
use rust_html_span_lib::parse;

pub fn method() {
    let html = r#"<span style="color: red;text-align:right" >hello world! I am <span style="color: blue; font-weight: bold;">18</span></span>"#;
    let html_info = parse(html);
    println!("{}", html_info);
}
```
