import gleam/list
import gleam/option
import gleam/pair
import lustre/attribute.{type Attribute}
import lustre/element as el
import lustre/internals/vdom
import sketch.{type StyleSheet}
import sketch/css

/// 1:1 representation of a `lustre/element.Element`.
///
/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#Element)
pub opaque type Element(msg) {
  Nothing
  Text(content: String)
  Map(subtree: fn() -> Element(msg))
  Element(
    key: String,
    namespace: String,
    tag: String,
    class: option.Option(css.Class),
    attributes: List(Attribute(msg)),
    children: List(Element(msg)),
  )
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#none)
pub fn none() -> Element(a) {
  Nothing
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#text)
pub fn text(content: String) -> Element(a) {
  Text(content)
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#element)
pub fn element(
  tag tag: String,
  class class: css.Class,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
) -> Element(msg) {
  let class = option.Some(class)
  Element("", "", tag, class, attributes, children)
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#element)
pub fn element_(
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
) -> Element(msg) {
  Element("", "", tag, option.None, attributes, children)
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#namespaced)
pub fn namespaced(
  namespace namespace: String,
  tag tag: String,
  class class: css.Class,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
) -> Element(msg) {
  let class = option.Some(class)
  Element("", namespace, tag, class, attributes, children)
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#namespaced)
pub fn namespaced_(
  namespace namespace: String,
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
) -> Element(msg) {
  Element("", namespace, tag, option.None, attributes, children)
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#fragment)
pub fn fragment(children: List(Element(msg))) -> Element(msg) {
  let attrs = [attribute.style([#("display", "contents")])]
  Element("", "", "lustre-fragment", option.None, attrs, children)
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#keyed)
pub fn keyed(
  element: fn(List(Element(msg))) -> Element(msg),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  element({
    use #(key, child) <- list.map(children)
    do_keyed(child, key)
  })
}

fn do_keyed(element: Element(msg), key: String) -> Element(msg) {
  case element {
    Nothing -> Nothing
    Text(content) -> Text(content)
    Map(subtree) -> Map(fn() { do_keyed(subtree(), key) })
    Element(_, namespace, tag, attributes, children, styles) ->
      Element(key, namespace, tag, attributes, children, styles)
  }
}

/// [Documentation](https://hexdocs.pm/lustre/lustre/element.html#map)
pub fn map(element: Element(a), mapper: fn(a) -> b) -> Element(b) {
  case element {
    Nothing -> Nothing
    Text(content) -> Text(content)
    Map(subtree) -> Map(fn() { map(subtree(), mapper) })
    Element(key, namespace, tag, class, attributes, children) -> {
      let attributes = list.map(attributes, attribute.map(_, mapper))
      let children = list.map(children, map(_, mapper))
      Element(key, namespace, tag, class, attributes, children)
    }
  }
}

/// Style a node from plain Lustre to Sketch. Use it when you need to convert
/// a foreign element in a `sketch_lustre` element.
pub fn styled(element: el.Element(msg)) -> Element(msg) {
  case element {
    vdom.Map(subtree) -> Map(fn() { styled(subtree()) })
    vdom.Text(content) -> Text(content)
    vdom.Element(key, namespace, tag, attrs, children, _, _) -> {
      let class = option.None
      Element(key, namespace, tag, class, attrs, list.map(children, styled))
    }
  }
}

/// Unstyle a node from Sketch to plain Lustre. Use it when you need to convert
/// a Sketch element in Lustre. You'll have to maintain the StyleSheet on your
/// own, though. \
/// Because of the nature of `unstyled`, that function returns a tuple, with the
/// updated stylesheet, and the Lustre element.
pub fn unstyled(
  stylesheet: StyleSheet,
  element: Element(msg),
) -> #(StyleSheet, el.Element(msg)) {
  case element {
    Nothing -> #(stylesheet, el.none())
    Text(content) -> #(stylesheet, el.text(content))
    Map(subtree) -> unstyled(stylesheet, subtree())
    Element(key, namespace, tag, class, attributes, children) -> {
      let class = option.map(class, sketch.class_name(_, stylesheet))
      let class_name = option.map(class, pair.second)
      let stylesheet =
        option.map(class, pair.first) |> option.unwrap(stylesheet)
      let #(stylesheet, children) = unstyled_children(stylesheet, children)
      let attributes = case class_name {
        option.None -> attributes
        option.Some(class_name) -> {
          let class_name = attribute.class(class_name)
          [class_name, ..attributes]
        }
      }
      #(stylesheet, case el.element(tag, attributes, children) {
        vdom.Element(_, _, t, a, c, s, v) ->
          vdom.Element(key, namespace, t, a, c, s, v)
        e -> e
      })
    }
  }
}

fn unstyled_children(
  stylesheet: StyleSheet,
  children: List(Element(msg)),
) -> #(StyleSheet, List(el.Element(msg))) {
  list.fold(list.reverse(children), #(stylesheet, []), fn(acc, child) {
    let #(stylesheet, children) = acc
    let #(stylesheet, child) = unstyled(stylesheet, child)
    #(stylesheet, [child, ..children])
  })
}
