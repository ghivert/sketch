//// This module is a drop-in replacement for `lustre/element/keyed`. Just
//// use the new functions, and everything will automagically be styled.

import lustre/attribute.{type Attribute}
import lustre/element.{type Element} as _
import lustre/element/keyed
import sketch/css
import sketch/lustre/experimental/element.{class_name}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#element)
pub fn element(
  tag: String,
  class: css.Class,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let attributes = case class_name(class) {
    Ok(class_name) -> [attribute.class(class_name), ..attributes]
    Error(_) -> attributes
  }
  keyed.element(tag, attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#element)
pub fn element_(
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  keyed.element(tag, attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#namespaced)
pub fn namespaced(
  namespace: String,
  tag: String,
  class: css.Class,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let attributes = case class_name(class) {
    Ok(class_name) -> [attribute.class(class_name), ..attributes]
    Error(_) -> attributes
  }
  keyed.namespaced(namespace, tag, attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#namespaced)
pub fn namespaced_(
  namespace: String,
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  keyed.namespaced(namespace, tag, attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#fragment)
pub const fragment = keyed.fragment

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#ul)
pub fn ul(
  class: css.Class,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let attributes = case class_name(class) {
    Ok(class_name) -> [attribute.class(class_name), ..attributes]
    Error(_) -> attributes
  }
  keyed.element("ul", attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#ul)
pub fn ul_(
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  keyed.element("ul", attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#ol)
pub fn ol(
  class: css.Class,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let attributes = case class_name(class) {
    Ok(class_name) -> [attribute.class(class_name), ..attributes]
    Error(_) -> attributes
  }
  keyed.element("ol", attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#ol)
pub fn ol_(
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  keyed.element("ol", attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#div)
pub fn div(
  class: css.Class,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let attributes = case class_name(class) {
    Ok(class_name) -> [attribute.class(class_name), ..attributes]
    Error(_) -> attributes
  }
  keyed.element("div", attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#div)
pub fn div_(
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  keyed.element("div", attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#tbody)
pub fn tbody(
  class: css.Class,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let attributes = case class_name(class) {
    Ok(class_name) -> [attribute.class(class_name), ..attributes]
    Error(_) -> attributes
  }
  keyed.element("tbody", attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#tbody)
pub fn tbody_(
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  keyed.element("tbody", attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#dl)
pub fn dl(
  class: css.Class,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let attributes = case class_name(class) {
    Ok(class_name) -> [attribute.class(class_name), ..attributes]
    Error(_) -> attributes
  }
  keyed.element("dl", attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element/keyed.html#dl)
pub fn dl_(
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  keyed.element("dl", attributes, children)
}
