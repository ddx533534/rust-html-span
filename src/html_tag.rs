use html5ever::{local_name, QualName};

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum HtmlTag {
    P = 0,
    Span = 1,
    Br = 2,
    B = 3,
    Strong = 4,
    I = 5,
    Em = 6,
    Div = 7,
}

impl From<&QualName> for HtmlTag {
    fn from(value: &QualName) -> Self {
        // NOTE: 使用 `local_name!` 会检查标签名是否在定义中存在同时使用编译时缓存的一个 Atom（优化的String）。
        match value.local {
            local_name!("p") => HtmlTag::P,
            local_name!("span") => HtmlTag::Span,
            local_name!("br") => HtmlTag::Br,
            local_name!("b") => HtmlTag::B,
            local_name!("strong") => HtmlTag::Strong,
            local_name!("i") => HtmlTag::I,
            local_name!("em") => HtmlTag::Em,
            local_name!("div") => HtmlTag::Div,
            _ => HtmlTag::Span,
        }
    }
}