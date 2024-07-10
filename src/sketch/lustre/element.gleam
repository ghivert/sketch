import gleam/list
import lustre/attribute.{type Attribute}
import lustre/element.{type Element}
import sketch.{type Style} as s

/// Equivalent to
/// ```gleam
/// fn component() {
///   let class =
///     sketch.class(styles)
///     |> sketch.to_lustre
///   element.element(tag, [class, ..attributes], children)
/// }
/// ```
pub fn element(
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
  styles styles: List(Style(media, pseudo_selector)),
) {
  s.class(styles)
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element(tag, _, children)
}

/// Equivalent to
/// ```gleam
/// fn component() {
///   let class =
///     sketch.class(styles)
///     |> sketch.memo
///     |> sketch.to_lustre
///   element.element(tag, [class, ..attributes], children)
/// }
/// ```
pub fn memo(
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
  styles styles: List(Style(media, pseudo_selector)),
) {
  s.class(styles)
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element(tag, _, children)
}

/// Equivalent to
/// ```gleam
/// fn component() {
///   let class =
///     sketch.dynamic(id, styles)
///     |> sketch.to_lustre
///   element.element(tag, [class, ..attributes], children)
/// }
/// ```
pub fn dynamic(
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
  id id: String,
  styles styles: List(Style(media, pseudo_selector)),
) {
  s.dynamic(id, styles)
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element(tag, _, children)
}

/// Equivalent to
/// ```gleam
/// fn component() {
///   let class =
///     sketch.dynamic(id, styles)
///     |> sketch.memo
///     |> sketch.to_lustre
///   element.element(tag, [class, ..attributes], children)
/// }
/// ```
pub fn memo_dynamic(
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
  id id: String,
  styles styles: List(Style(media, pseudo_selector)),
) {
  s.dynamic(id, styles)
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element(tag, _, children)
}
