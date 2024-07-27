import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import lustre/attribute.{type Attribute}
import lustre/element as lel
import lustre/internals/vdom
import sketch

pub opaque type Element(msg) {
  NoNode
  Text(content: String)
  Fragment(key: String, children: List(Element(msg)))
  Element(
    key: String,
    tag: String,
    attributes: List(Attribute(msg)),
    children: List(Element(msg)),
    styles: List(sketch.Style),
  )
}

pub fn none() {
  NoNode
}

pub fn text(content) {
  Text(content)
}

pub fn element(
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
  styles styles: List(sketch.Style),
) {
  Element("", tag, attributes, children, styles)
}

pub fn fragment(children: List(Element(msg))) {
  Fragment("", children)
}

pub fn keyed(
  el: fn(List(Element(msg))) -> Element(msg),
  children: List(#(String, Element(msg))),
) {
  el(
    list.map(children, fn(c) {
      case c.1 {
        NoNode -> NoNode
        Text(t) -> Text(t)
        Fragment(_, e) -> Fragment(c.0, e)
        Element(_, t, a, c_, s) -> Element(c.0, t, a, c_, s)
      }
    }),
  )
}

pub fn map(el: Element(a), f: fn(a) -> b) {
  case el {
    NoNode -> NoNode
    Text(c) -> Text(c)
    Fragment(k, c) -> Fragment(k, list.map(c, map(_, f)))
    Element(k, t, a, c, s) ->
      Element(k, t, list.map(a, attribute.map(_, f)), list.map(c, map(_, f)), s)
  }
}

pub fn unstylify(
  cache: sketch.Cache,
  element: Element(msg),
) -> #(sketch.Cache, lel.Element(msg)) {
  case element {
    NoNode -> #(cache, lel.none())
    Text(content) -> #(cache, lel.text(content))
    Fragment(k, children) ->
      unstylify_children(cache, children)
      |> pair.map_second(fn(content) { vdom.Fragment(content, k) })
    Element(k, tag, attributes, children, styles) -> {
      let class = sketch.class(styles)
      let #(cache, class_name) = sketch.class_name(class, cache)
      let #(cache, children) = unstylify_children(cache, children)
      let class_name = attribute.class(class_name)
      let attributes = [class_name, ..attributes]
      #(cache, case lel.element(tag, attributes, children) {
        vdom.Element(_, n, t, a, c, s, v) -> vdom.Element(k, n, t, a, c, s, v)
        e -> e
      })
    }
  }
}

fn unstylify_children(cache, children) {
  list.fold_right(children, #(cache, []), fn(acc, child) {
    let #(cache, children) = acc
    let #(cache, child) = unstylify(cache, child)
    #(cache, [child, ..children])
  })
}
