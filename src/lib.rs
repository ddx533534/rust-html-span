use std::{cmp, fmt, mem};
use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::Range;
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicU32, Ordering};

use html5ever::{
    Attribute, ExpandedName, local_name, LocalName, Namespace, parse_fragment, ParseOpts, Prefix,
    QualName,
};
use html5ever::interface::{ElementFlags, NodeOrText, QuirksMode, tree_builder, TreeSink};
use html5ever::tendril::{StrTendril, TendrilSink};
use html5ever::tree_builder::TreeBuilderOpts;

pub use html_tag::*;

mod html_tag;

#[derive(Debug)]
pub enum NodeData {
    /// The `Document` itself - the root node of an HTML document.
    Document,

    /// A `DOCTYPE` with name, public id, and system id. See
    /// [document type declaration on wikipedia][dtd wiki].
    ///
    /// [dtd wiki]: https://en.wikipedia.org/wiki/Document_type_declaration
    Doctype {
        name: StrTendril,
        public_id: StrTendril,
        system_id: StrTendril,
    },

    /// A text node.
    Text { contents: RefCell<StrTendril> },

    /// A comment.
    Comment { contents: StrTendril },

    /// An element with attributes.
    Element {
        name: QualName,
        attrs: RefCell<Vec<Attribute>>,

        /// For HTML \<template\> elements, the [template contents].
        ///
        /// [template contents]: https://html.spec.whatwg.org/multipage/#template-contents
        template_contents: RefCell<Option<Handle>>,

        /// Whether the node is a [HTML integration point].
        ///
        /// [HTML integration point]: https://html.spec.whatwg.org/multipage/#html-integration-point
        mathml_annotation_xml_integration_point: bool,
    },

    /// A Processing instruction.
    ProcessingInstruction {
        target: StrTendril,
        contents: StrTendril,
    },
}

/// A DOM node.
pub struct Node {
    /// Parent node, weak reference.
    pub parent: Cell<Option<WeakHandle>>,
    /// Child nodes of this node.
    pub children: RefCell<Vec<Handle>>,
    /// Represents this node's data.
    pub data: NodeData,
    /// Represents unique tag of Node
    pub tag: u32,
}

impl Node {
    /// Create a new node from its contents
    pub fn new(data: NodeData, tag: u32) -> Rc<Self> {
        Rc::new(Node {
            parent: Cell::new(None),
            children: RefCell::new(Vec::new()),
            data,
            tag,
        })
    }
}

impl Drop for Node {
    fn drop(&mut self) {
        let mut nodes = mem::take(&mut *self.children.borrow_mut());
        while let Some(node) = nodes.pop() {
            let children = mem::take(&mut *node.children.borrow_mut());
            nodes.extend(children.into_iter());
            if let NodeData::Element {
                ref template_contents,
                ..
            } = node.data
            {
                if let Some(template_contents) = template_contents.borrow_mut().take() {
                    nodes.push(template_contents);
                }
            }
        }
    }
}

impl Debug for Node {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Node")
            .field("data", &self.data)
            .field("children", &self.children)
            .finish()
    }
}

/// Reference to a DOM node.
pub type Handle = Rc<Node>;

/// Weak reference to a DOM node, used for parent pointers.
pub type WeakHandle = Weak<Node>;

#[derive(Debug)]
pub struct TextSpanInfo {
    /// represents which element node it belongs by tag
    id: u32,
    /// text content
    content: String,
    /// text span range
    range: Range<usize>,
    /// represents whether is virtual,if two or more text node has the same parent, they correspond to text_span are all virtual except the first.
    is_virtual: bool,
    /// represents element node type it belongs to
    html_tag: HtmlTag,
    /// 记录在 Html 标签上的属性（比如 font、style）
    attrs: Vec<Attribute>,
}

pub const ROOT_TAG: u32 = 0;

trait UniqueId {
    fn get_unique_id(&self) -> u32;
}

impl UniqueId for Handle {
    fn get_unique_id(&self) -> u32 {
        self.tag
    }
}

fn is_ascii_whitespace(c: char) -> bool {
    match c {
        ' ' | '\x09'..='\x0d' => true,
        _ => false,
    }
}

/// 处理 Html 中的文本，规范空格和换行
fn normalize_html_text(has_space: bool, text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let mut in_whitespace = has_space; // 如果之前的文本末尾是空格，则从空格开始计算
    for c in text.chars() {
        // 不要使用 is_whitespace，不要处理 unicode 中的空格类型
        if is_ascii_whitespace(c) {
            if !in_whitespace {
                result.push(' ');
                in_whitespace = true;
            }
        } else {
            result.push(c);
            in_whitespace = false;
        }
    }
    result
}

pub struct HtmlTreeSink {
    /// The `Document` itself
    pub document: Handle,
    /// Errors that occurred during parsing.
    pub errors: Vec<Cow<'static, str>>,
    /// The document's quirks mode.
    pub quirks_mode: QuirksMode,
    /// text span infos
    pub text_span_infos: Vec<TextSpanInfo>,
    /// parent child relationship
    pub map: HashMap<u32, u32>,
    /// responsible for assigning tag of node
    pub tag_distributor: AtomicU32,
    /// 拼接的完整文本
    pub full_text: String,
    /// debug mode could show more info
    pub debug_mode: bool,
}

impl HtmlTreeSink {
    pub fn default() -> Self {
        HtmlTreeSink {
            document: Node::new(NodeData::Document, ROOT_TAG),
            errors: vec![],
            quirks_mode: tree_builder::NoQuirks,
            text_span_infos: vec![],
            map: HashMap::new(),
            tag_distributor: AtomicU32::new(1),
            full_text: String::new(),
            debug_mode: true,
        }
    }
    pub fn set_debug_mode(&mut self, debug: bool) {
        self.debug_mode = debug;
    }

    pub fn get_formated_spans(&mut self) -> String {
        // format info！！！
        format!("{:?}", self.text_span_infos)
    }

    fn allocate_new_tag(&mut self) -> u32 {
        self.tag_distributor.fetch_add(1, Ordering::SeqCst)
    }

    /// 处理添加文本类型的场景：
    /// 1.处理空格和换行
    /// 2.建立附属于 parent 节点的 text_span_info
    /// 3.向上不断更新父节点的 text_span_info
    fn process_append_text(&mut self, text: &str, need_to_normalize: bool, parent: &Handle) {
        let last_is_whitespace = self.full_text.ends_with(' ');
        let text = if need_to_normalize {
            let mut text = normalize_html_text(last_is_whitespace, text);
            if !self.full_text.is_empty() && parent.children.borrow().is_empty() {
                if let NodeData::Element { name, .. } = &parent.data {
                    // 为块级元素添加换行
                    match name.local {
                        local_name!("div") | local_name!("p") => {
                            if last_is_whitespace {
                                // 如果当前最后一个字符是空格则替换为换行符，换行的归属可能不重要
                                self.full_text.replace_range(
                                    (self.full_text.len() - 1)..self.full_text.len(),
                                    "\n",
                                );
                            } else {
                                text.insert(0, '\n');
                            }
                        }
                        _ => {}
                    };
                }
            }
            text
        } else {
            text.to_string()
        };
        let text = text.as_str();
        let start = self.text_span_infos.last().map_or(0, |span| span.range.end);
        let end = start + text.chars().count();
        // identify which node tag the text belongs to
        let id = parent.get_unique_id();
        // if there is an element in the text_span_infos collection with an equal tag, it is virtual
        let is_virtual = self.is_text_span_virtual(id);
        let (node_type, attrs) = match &parent.data {
            NodeData::Element { name, attrs, .. } => (HtmlTag::from(name), attrs.take().clone()),
            _ => (HtmlTag::Span, Vec::new()),
        };
        // 此处直接创建，因为每个文本一定要有一个 text_span_info，通过 is_virtual 来区分！
        self.text_span_infos.push(TextSpanInfo {
            id,
            content: text.to_string(),
            range: start..end,
            is_virtual,
            html_tag: node_type,
            attrs,
        });
        self.update_parent_text_span_info(parent, start, end, text);
        if is_virtual {
            self.merge_text_span(parent, start, end, text);
        }
        self.full_text.push_str(text);
    }

    /// whether a text_span is virtual, by determining whether the text  has the same element node id
    fn is_text_span_virtual(&self, p_id: u32) -> bool {
        self.text_span_infos.iter().any(|span| span.id == p_id)
    }

    /// 处理添加节点类型的场景：
    /// 1.标记 br 标签
    /// 2.建立附属于 parent 节点的 text_span_info
    /// 3.更新当前父子关系，建立索引
    fn process_append_node(&mut self, child: &Handle, parent: &Handle) {
        if let NodeData::Element { name, .. } = &child.data {
            //1. br 标记转换为一个换行文本
            if name.local == local_name!("br") {
                if self.full_text.ends_with(' ') {
                    self.full_text
                        .replace_range((self.full_text.len() - 1)..self.full_text.len(), "\n");
                } else {
                    self.process_append_text("\n", false, parent);
                }
            }
        }

        // 2.创建附属于 parent 的 text_span_info，如果已经有子节点说明已经创建过，不再重复创建!
        let is_parent_empty = parent.children.borrow().is_empty();
        if is_parent_empty {
            // node 添加到 node 的场景，文本是长度为0的字符串切片
            let text = "";
            self.process_append_text(text, true, parent);
        }
        // 3.建立父子索引
        self.map
            .insert(child.get_unique_id(), parent.get_unique_id());
    }

    /// iteratively update the text_span infos for the parent text.
    /// for example(unfold): when encounter text "ld",we need iteratively update the "wor" text_span and "hello" text_span
    ///                div
    ///              /   |
    ///         "hello" div
    ///                  / \
    ///              "wor"  div
    ///                 /
    ///               "ld"
    fn update_parent_text_span_info(
        &mut self,
        handle: &Handle,
        start: usize,
        end: usize,
        text: &str,
    ) {
        let mut tag = handle.get_unique_id();
        // convert vec into map, we just retains the non-virtual text_span to avoid duplicate key!
        let mut span_map: HashMap<u32, &mut TextSpanInfo> = self
            .text_span_infos
            .iter_mut()
            .filter(|span| !span.is_virtual)
            .map(|span| (span.id, span))
            .collect();
        while let Some(p_tag) = self.map.get(&tag) {
            if let Some(text_span) = span_map.get_mut(p_tag) {
                text_span.content.push_str(text);
                text_span.range.start = cmp::min(text_span.range.start, start);
                text_span.range.end = cmp::max(text_span.range.end, end);
            }
            if *p_tag == tag {
                // avoiding infinite loop
                break;
            }
            tag = *p_tag;
        }
    }

    /// merge text info into previous text info which has the same parent!
    /// for example(unfold): when encounter the text "!" ,we need merge its text_span info into the "hello" text_span
    ///                 div
    ///              /   |   \
    ///         "hello" div "!"
    ///                  /
    ///                "world"
    fn merge_text_span(&mut self, handle: &Handle, start: usize, end: usize, text: &str) {
        let p_id = handle.get_unique_id();
        if let Some(text_span) = self
            .text_span_infos
            .iter_mut()
            .find(|span| span.id == p_id)
        {
            text_span.content.push_str(text);
            text_span.range.start = cmp::min(text_span.range.start, start);
            text_span.range.end = cmp::max(text_span.range.end, end);
        }
    }
}

fn append(new_parent: &Handle, child: Handle) {
    let previous_parent = child.parent.replace(Some(Rc::downgrade(new_parent)));
    // Invariant: child cannot have existing parent
    assert!(previous_parent.is_none());
    new_parent.children.borrow_mut().push(child);
}

#[allow(unused)]
fn append_to_existing_text(prev: &Handle, text: &str) -> bool {
    match prev.data {
        NodeData::Text { ref contents } => {
            contents.borrow_mut().push_slice(text);
            true
        }
        _ => false,
    }
}

impl TreeSink for HtmlTreeSink {
    type Handle = Handle;
    type Output = HtmlTreeSink;

    fn finish(self) -> Self::Output {
        self
    }

    fn parse_error(&mut self, msg: Cow<'static, str>) {
        self.errors.push(msg);
    }

    fn get_document(&mut self) -> Self::Handle {
        self.document.clone()
    }

    fn elem_name<'a>(&'a self, target: &'a Self::Handle) -> ExpandedName<'a> {
        return match target.data {
            NodeData::Element { ref name, .. } => name.expanded(),
            _ => panic!("not an element!"),
        };
    }

    fn create_element(
        &mut self,
        name: QualName,
        attrs: Vec<Attribute>,
        flags: ElementFlags,
    ) -> Self::Handle {
        Node::new(
            NodeData::Element {
                name,
                attrs: RefCell::new(attrs),
                template_contents: RefCell::new(if flags.template {
                    Some(Node::new(NodeData::Document, self.allocate_new_tag()))
                } else {
                    None
                }),
                mathml_annotation_xml_integration_point: flags
                    .mathml_annotation_xml_integration_point,
            },
            self.allocate_new_tag(),
        )
    }

    fn create_comment(&mut self, text: StrTendril) -> Self::Handle {
        Node::new(
            NodeData::Comment { contents: text },
            self.allocate_new_tag(),
        )
    }

    fn create_pi(&mut self, target: StrTendril, data: StrTendril) -> Self::Handle {
        Node::new(
            NodeData::ProcessingInstruction {
                target,
                contents: data,
            },
            self.allocate_new_tag(),
        )
    }

    fn append(&mut self, parent: &Self::Handle, child: NodeOrText<Self::Handle>) {
        match child {
            NodeOrText::AppendText(text) => {
                self.process_append_text(&text, true, parent);
                append(
                    parent,
                    Node::new(
                        NodeData::Text {
                            contents: RefCell::new(text),
                        },
                        self.allocate_new_tag(),
                    ),
                );
            }
            NodeOrText::AppendNode(node) => {
                self.process_append_node(&node, parent);
                append(parent, node);
            }
        };
    }

    #[allow(unused)]
    fn append_based_on_parent_node(
        &mut self,
        element: &Self::Handle,
        prev_element: &Self::Handle,
        child: NodeOrText<Self::Handle>,
    ) {
        //no implement
    }

    #[allow(unused)]
    fn append_doctype_to_document(
        &mut self,
        name: StrTendril,
        public_id: StrTendril,
        system_id: StrTendril,
    ) {
        //no implement
    }

    fn get_template_contents(&mut self, target: &Self::Handle) -> Self::Handle {
        if let NodeData::Element {
            ref template_contents,
            ..
        } = target.data
        {
            template_contents
                .borrow()
                .as_ref()
                .expect("not a template element!")
                .clone()
        } else {
            panic!("not a template element!")
        }
    }

    fn same_node(&self, x: &Self::Handle, y: &Self::Handle) -> bool {
        Rc::ptr_eq(x, y)
    }

    fn set_quirks_mode(&mut self, mode: QuirksMode) {
        self.quirks_mode = mode;
    }

    #[allow(unused)]
    fn append_before_sibling(
        &mut self,
        sibling: &Self::Handle,
        new_node: NodeOrText<Self::Handle>,
    ) {
        //no implement
    }

    #[allow(unused)]
    fn add_attrs_if_missing(&mut self, target: &Self::Handle, attrs: Vec<Attribute>) {
        //no implement
    }

    #[allow(unused)]
    fn remove_from_parent(&mut self, target: &Self::Handle) {
        //no implement
    }

    #[allow(unused)]
    fn reparent_children(&mut self, node: &Self::Handle, new_parent: &Self::Handle) {
        //no implement
    }
}

// #[derive(Debug, Clone)]
// pub struct StyleAttribute(PropertyDeclaration);
//
// impl Deref for StyleAttribute {
//     type Target = PropertyDeclaration;
//
//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }
//
// impl StyleAttribute {
//     pub fn new(declaration: PropertyDeclaration) -> Self {
//         StyleAttribute(declaration)
//     }
// }

// TODO: 这个保存直接设置在标签上的样式
#[derive(Debug, Clone)]
pub struct TextAttribute {}

/// 保存一个 Tag 应用的范围和属性（叫啥名字更合适呢？）
#[derive(Debug, Clone)]
pub struct HtmlTagRange {
    pub id: u32,
    pub parent_id: u32,
    pub tag: HtmlTag,
    pub range: Range<usize>,
    pub attributes: Vec<Attribute>,
    // pub text_attributes: Vec<TextAttribute>,
}

/// 最终输出给外部使用的信息
#[derive(Debug)]
pub struct HtmlInfo {
    pub text: String,
    pub ranges: Vec<HtmlTagRange>,
}

// pub fn parse_attributes(attributes: &[Attribute]) -> (Vec<StyleAttribute>, Vec<TextAttribute>) {
//     let mut style_attributes = Vec::new();
//     let text_attributes = Vec::new();
// 
//     for attr in attributes {
//         match attr.name.local {
//             local_name!("style") => {
//                 let mut input = ParserInput::new();
//                 let mut parser = recce_css::Parser::new(&mut input);
//                 if let Ok(decls) = recce_css::parse_to_decls(&mut parser) {
//                     style_attributes.extend(decls.into_iter().map(|p| StyleAttribute(p)));
//                 }
//             }
//             _ => {}
//         }
//     }
// 
//     (style_attributes, text_attributes)
// }

/// 解析一个 Html 片段为 HtmlInfo
pub fn parse(html: &str) -> HtmlInfo {
    let opts = ParseOpts {
        tree_builder: TreeBuilderOpts {
            drop_doctype: true,
            ..Default::default()
        },
        ..Default::default()
    };
    let html_tree_sink = HtmlTreeSink::default();
    // NOTE: parse_fragment 接口限制必须传入一个上下文节点，这里传一个假的
    let parser = parse_fragment(
        html_tree_sink,
        opts,
        QualName::new(
            Some(Prefix::from("dummy")),
            Namespace::from("dummy"),
            LocalName::from("root"),
        ),
        vec![],
    );
    let sink = parser.one(html);
    let ranges: Vec<_> = sink
        .text_span_infos
        .iter()
        .filter_map(|text_span_info| {
            if text_span_info.is_virtual || text_span_info.id == ROOT_TAG {
                return None;
            }
            // let (style_attributes, text_attributes) = parse_attributes(&i.attrs);
            let p_id = if let Some(id) = sink.map.get(&text_span_info.id) {
                *id
            } else {
                // 目前创建的 span 不会有 0 的场景，我们用 0 表示没有父节点
                0
            };
            Some(HtmlTagRange {
                id: text_span_info.id,
                parent_id: p_id,
                tag: text_span_info.html_tag,
                range: text_span_info.range.clone(),
                attributes: text_span_info.attrs.clone(),
            })
        })
        .collect();
    HtmlInfo {
        text: sink.full_text,
        ranges,
    }
}

impl fmt::Display for HtmlInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for item in &self.ranges {
            writeln!(
                f,
                "[id:{}, parent_id:{}, tag: {:?}, range:{:?}, attributes:{:?}]",
                item.id, item.parent_id, item.tag, item.range, item.attributes
            ).expect("display for HtmlInfo error!");
        }
        writeln!(f, "text: {}", self.text)
    }
}

#[cfg(test)]
mod test {
    use crate::parse;

    #[test]
    pub fn pure_text() {
        let html = r#"hello world!I am 18!"#;
        let html_info = parse(html);
        println!("{}", html_info);
        assert_eq!(html_info.ranges[0].range, 0..20);
    }

    #[test]
    pub fn no_root() {
        let html = r#"<span>hello world!</span><span>I am <b>18</b>!</span>"#;
        let html_info = parse(html);
        println!("{}", html_info);
        assert_eq!(html_info.ranges[0].range, 0..20);
        assert_eq!(html_info.ranges[1].range, 0..12);
        assert_eq!(html_info.ranges[2].range, 12..20);
        assert_eq!(html_info.ranges[3].range, 17..19);
    }

    #[test]
    pub fn normal_fragment() {
        let html = r#"<span>hello world!I am <b>18</b>!</span>"#;
        let html_info = parse(html);
        println!("{}", html_info);
        assert_eq!(html_info.ranges[0].range, 0..20);
        assert_eq!(html_info.ranges[1].range, 0..20);
        assert_eq!(html_info.ranges[2].range, 17..19);
    }

    #[test]
    pub fn space() {
        let space_html = r#"<p>    </span>"#;
        assert_eq!(parse(space_html).text, " ");
        let nbsp_html = r#"<p>&nbsp;&nbsp;&nbsp;&nbsp;</span>"#;
        assert_eq!(parse(nbsp_html).text, "\u{a0}\u{a0}\u{a0}\u{a0}");
        let ensp_html = r#"<p>&ensp;&ensp;&ensp;&ensp;</span>"#;
        assert_eq!(parse(ensp_html).text, "\u{2002}\u{2002}\u{2002}\u{2002}");
        let emsp_html = r#"<p>&emsp;&emsp;&emsp;&emsp;</span>"#;
        assert_eq!(parse(emsp_html).text, "\u{2003}\u{2003}\u{2003}\u{2003}");
    }

    #[test]
    pub fn line_break() {
        let n_html = "<span>hello\nworld!</span>";
        assert_eq!(parse(n_html).text, "hello world!");
        let br_html = r#"<p>hello<br>world!<br></p>"#;
        assert_eq!(parse(br_html).text, "hello\nworld!\n");
        let div_html = r#"<div>hello</div><div>world!</div>"#;
        assert_eq!(parse(div_html).text, "hello\nworld!");
        let p_html = r#"<p>hello</p><p>world!</p>"#;
        assert_eq!(parse(p_html).text, "hello\nworld!");
    }

    #[test]
    pub fn attributes() {
        let n_html = r#"<span style="color: red">hello world!</span>"#;
        println!("{}", parse(n_html));
    }
}