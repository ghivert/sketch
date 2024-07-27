import gleam/int
import gleam/list
import gleam/pair
import lustre/attribute.{type Attribute}
import lustre/element as el
import lustre/internals/vdom
import sketch.{type Cache}

pub opaque type Element(msg) {
  Nothing
  Text(content: String)
  Fragment(key: String, children: List(Element(msg)))
  Map(subtree: fn() -> Element(msg))
  Element(
    key: String,
    namespace: String,
    tag: String,
    attributes: List(Attribute(msg)),
    children: List(Element(msg)),
    styles: List(sketch.Style),
  )
}

pub fn none() {
  Nothing
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
  Element("", "", tag, attributes, children, styles)
}

pub fn namespaced(
  namespace namespace: String,
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
  styles styles: List(sketch.Style),
) {
  Element("", namespace, tag, attributes, children, styles)
}

pub fn fragment(children: List(Element(msg))) {
  Fragment("", children)
}

pub fn keyed(
  element: fn(List(Element(msg))) -> Element(msg),
  children: List(#(String, Element(msg))),
) {
  element({
    use #(key, child) <- list.map(children)
    do_keyed(child, key)
  })
}

fn do_keyed(element: Element(msg), key: String) {
  case element {
    Nothing -> Nothing
    Text(content) -> Text(content)
    Map(subtree) -> Map(fn() { do_keyed(subtree(), key) })
    Fragment(_, children) ->
      children
      |> list.index_map(fn(element, idx) {
        case element {
          Element(el_key, _, _, _, _, _) -> {
            let new_key = case el_key {
              "" -> key <> "-" <> int.to_string(idx)
              _ -> key <> "-" <> el_key
            }
            do_keyed(element, new_key)
          }
          _ -> do_keyed(element, key)
        }
      })
      |> Fragment(key, _)
    Element(_, tag, namespace, attributes, children, styles) ->
      Element(key, tag, namespace, attributes, children, styles)
  }
}

pub fn map(element: Element(a), mapper: fn(a) -> b) {
  case element {
    Nothing -> Nothing
    Text(content) -> Text(content)
    Map(subtree) -> Map(fn() { map(subtree(), mapper) })
    Fragment(key, children) -> Fragment(key, list.map(children, map(_, mapper)))
    Element(key, tag, namespace, attributes, children, styles) -> {
      let attributes = list.map(attributes, attribute.map(_, mapper))
      let children = list.map(children, map(_, mapper))
      Element(key, tag, namespace, attributes, children, styles)
    }
  }
}

pub fn styled(element: el.Element(msg)) {
  case element {
    vdom.Map(subtree) -> Map(fn() { styled(subtree()) })
    vdom.Text(content) -> Text(content)
    vdom.Fragment(elements, key) -> Fragment(key, list.map(elements, styled))
    vdom.Element(key, namespace, tag, attrs, children, _, _) ->
      Element(key, namespace, tag, attrs, list.map(children, styled), [])
  }
}

pub fn unstyled(cache: Cache, element: Element(msg)) {
  case element {
    Nothing -> #(cache, el.none())
    Text(content) -> #(cache, el.text(content))
    Map(subtree) -> unstyled(cache, subtree())
    Fragment(key, children) ->
      unstyled_children(cache, children)
      |> pair.map_second(fn(node) { vdom.Fragment(node, key) })
    Element(key, tag, namespace, attributes, children, styles) -> {
      let class = sketch.class(styles)
      let #(cache, class_name) = sketch.class_name(class, cache)
      let #(cache, children) = unstyled_children(cache, children)
      let class_name = attribute.class(class_name)
      let attributes = [class_name, ..attributes]
      #(cache, case el.element(tag, attributes, children) {
        vdom.Element(_, _, t, a, c, s, v) ->
          vdom.Element(key, namespace, t, a, c, s, v)
        e -> e
      })
    }
  }
}

fn unstyled_children(cache, children) {
  list.fold_right(children, #(cache, []), fn(acc, child) {
    let #(cache, children) = acc
    let #(cache, child) = unstyled(cache, child)
    #(cache, [child, ..children])
  })
}
