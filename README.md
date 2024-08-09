# rust-html-span

use rust library **html5ever** to transfer html fragment to text spans!
1. base spanInfos ✓
2. support line-break and space ✓
3. support properties pase x

for example:
```shell
html: <span>hello world!</span><span>I am <b>18</b>!</span>
transfer result:
[id:2, parent_id:0, tag: Span, range:0..20, attributes:[]]
[id:3, parent_id:2, tag: Span, range:0..20, attributes:[]]
[id:5, parent_id:3, tag: B, range:17..19, attributes:[]]
text: hello world!I am 18!
```

usage:
```shell
cargo run
```