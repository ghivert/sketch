import gleam/dynamic.{type Dynamic}
import gleam/list
import lustre/element as el
import lustre/element/html
import lustre/internals/vdom
import plinth/browser/shadow.{type ShadowRoot}
import sketch.{type Cache}
import sketch/internals/ffi
import sketch/lustre/element

@external(javascript, "../sketch_lustre.ffi.mjs", "createCssStyleSheet")
fn create_document_stylesheet() -> Dynamic

@external(javascript, "../sketch_lustre.ffi.mjs", "createCssStyleSheet")
fn create_shadow_root_stylesheet(root: ShadowRoot) -> Dynamic

@external(javascript, "../sketch_lustre.ffi.mjs", "setStylesheet")
fn set_stylesheet(content: String, stylesheet: Dynamic) -> Nil {
  Nil
}

type StyleSheetOption {
  Node
  Document
  Shadow(root: ShadowRoot)
}

/// Options to indicate where to output the StyleSheet.
/// Can be a Node in DOM, a `CSSStyleSheet` in `document` or in a shadow root.
pub opaque type Options {
  Options(stylesheet: StyleSheetOption)
}

type StyleSheet {
  CssStyleSheet(Dynamic)
  NodeStyleSheet
}

/// Wrap the view function in lustre. Be careful, on BEAM, sketch will add an
/// additional `div` at the root of the HTML tree, to inject the styles in the
/// app, currently due to a fragment bug.
/// This should have no impact on your app.
pub fn compose(
  options: Options,
  view: fn(model) -> element.Element(msg),
  cache: Cache,
) {
  let cache = ffi.wrap(cache)
  let stylesheet = to_stylesheet(options)
  fn(model: model) -> el.Element(msg) {
    let node = view(model)
    let #(result, node) = element.unstyled(ffi.get(cache), node)
    let content = sketch.render(result)
    ffi.set(cache, result)
    render_stylesheet(content, node, stylesheet)
  }
}

@target(erlang)
fn to_stylesheet(options) {
  NodeStyleSheet
}

@target(javascript)
fn to_stylesheet(options) {
  case options {
    Options(Node) -> NodeStyleSheet
    Options(Document) -> CssStyleSheet(create_document_stylesheet())
    Options(Shadow(root)) -> CssStyleSheet(create_shadow_root_stylesheet(root))
  }
}

fn render_stylesheet(content, node, stylesheet) {
  case stylesheet {
    NodeStyleSheet -> root([el.element("style", [], [el.text(content)]), node])
    CssStyleSheet(stylesheet) -> {
      set_stylesheet(content, stylesheet)
      node
    }
  }
}

@target(javascript)
fn root(children) {
  el.fragment(children)
}

@target(erlang)
fn root(children) {
  html.div([], children)
}

fn contains_head(el: el.Element(a)) {
  case el {
    vdom.Element(_, _, "head", _, _, _, _) -> True
    vdom.Element(_, _, _, _, children, _, _) ->
      list.fold(children, False, fn(acc, val) { acc || contains_head(val) })
    _ -> False
  }
}

fn put_in_head(el: el.Element(a), content: String) {
  case el {
    vdom.Element(k, n, "head", a, children, s, v) ->
      children
      |> list.append([html.style([], content)])
      |> vdom.Element(k, n, "head", a, _, s, v)
    vdom.Element(k, n, "html", a, children, s, v) ->
      children
      |> list.map(fn(child) { put_in_head(child, content) })
      |> vdom.Element(k, n, "html", a, _, s, v)
    node -> node
  }
}

@target(erlang)
/// Take an Element, and overloads the content with the correct styles from sketch.
/// Can only be used on BEAM.
pub fn ssr(el: element.Element(a), cache: Cache) -> el.Element(a) {
  let #(cache, el) = element.unstyled(cache, el)
  let stylesheet = sketch.render(cache)
  case contains_head(el) {
    True -> put_in_head(el, stylesheet)
    False -> el.fragment([html.style([], stylesheet), el])
  }
}

/// Output the StyleSheet in a `style` tag in DOM.
pub fn node() -> Options {
  Options(stylesheet: Node)
}

/// Output the StyleSheet in a `CSSStyleSheet` in `document`.
/// `document` cannot be used on server.
pub fn document() -> Options {
  Options(stylesheet: Document)
}

/// Output the StyleSheet in a `CSSStyleSheet` in a shadow root.
/// `shadow` cannot be used on server.
pub fn shadow(root: ShadowRoot) -> Options {
  Options(stylesheet: Shadow(root: root))
}
