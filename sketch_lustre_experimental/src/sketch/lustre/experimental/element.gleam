import lustre/attribute.{type Attribute}
import lustre/element as el
import sketch
import sketch/css
import sketch/lustre/experimental/internals/global

/// Alias for `lustre/element.Element`. \
/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#Element)
pub type Element(msg) =
  el.Element(msg)

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#keyed)
pub const keyed = el.keyed

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#fragment)
pub const fragment = el.fragment

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#none)
pub const none = el.none

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#text)
pub const text = el.text

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#map)
pub const map = el.map

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#element)
pub fn element(
  tag tag: String,
  class class: css.Class,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  let class_name = class_name(class)
  el.element(tag, [attribute.class(class_name), ..attributes], children)
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#element)
pub fn element_(
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  el.element(tag, attributes, children)
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#namespaced)
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

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#namespaced)
pub fn namespaced_(
  tag tag: String,
  namespace namespace: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  el.namespaced(tag, namespace, attributes, children)
}

/// Generate a class name from a `Class`, using the `StyleSheet` injected
/// in the environment.
pub fn class_name(class: css.Class) -> String {
  let assert Ok(stylesheet) = global.get_stylesheet()
  let #(stylesheet, class_name) = sketch.class_name(class, stylesheet)
  let _ = global.set_stylesheet(stylesheet)
  class_name
}
