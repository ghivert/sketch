//// This module is a drop-in replacement for `lustre/element`. Just
//// use the new functions, and everything will automagically be styled.

import gleam/io
import lustre/attribute.{type Attribute}
import lustre/element as el
import sketch
import sketch/css
import sketch/lustre/experimental/internals/global

/// Alias for `lustre/element.Element`. \
/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element.html#Element)
pub type Element(msg) =
  el.Element(msg)

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element.html#fragment)
pub const fragment = el.fragment

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element.html#none)
pub const none = el.none

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element.html#text)
pub const text = el.text

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element.html#map)
pub const map = el.map

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element.html#element)
pub fn element(
  tag tag: String,
  class class: css.Class,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  let class_name = class_name(class)
  let attributes = [attribute.class(class_name), ..attributes]
  el.element(tag, attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element.html#element)
pub fn element_(
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  el.element(tag, attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element.html#namespaced)
pub fn namespaced(
  tag tag: String,
  namespace namespace: String,
  class class: css.Class,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  let class_name = class_name(class)
  let attributes = [attribute.class(class_name), ..attributes]
  el.namespaced(tag, namespace, attributes, children)
}

/// [Lustre Documentation](https://hexdocs.pm/lustre/lustre/element.html#namespaced)
pub fn namespaced_(
  tag tag: String,
  namespace namespace: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  el.namespaced(tag, namespace, attributes, children)
}

const error_msg = "Stylesheet is not initialized in your application. Please, initialize a stylesheet before rendering some nodes."

/// Generate a class name from a `Class`, using the `StyleSheet` injected
/// in the environment.
pub fn class_name(class: css.Class) -> String {
  case global.get_stylesheet() {
    Error(_) -> panic as error_msg
    Ok(stylesheet) -> {
      let #(stylesheet, class_name) = sketch.class_name(class, stylesheet)
      let _ = global.set_stylesheet(stylesheet)
      class_name
    }
  }
}
