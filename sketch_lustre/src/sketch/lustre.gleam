import gleam/dynamic.{type Dynamic}
import gleam/list
import lustre/element as el
import lustre/element/html
import lustre/internals/vdom
import sketch
import sketch/internals/css_stylesheet as stylesheet
import sketch/internals/mutable
import sketch/lustre/element

type StyleSheetOption {
  Node
  Document
  Shadow(root: Dynamic)
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

/// Use `render` as a view middleware. Like in Wisp, `sketch_lustre` adopts the
/// middleware philosophy in Lustre, and allows you to call `render` directly
/// in your view function, by using `use`. No need to wrap your view function.
///
/// ```gleam
/// import lustre
/// import sketch
/// import sketch/lustre as sketch_lustre
/// import sketch/lustre/element/html
///
/// pub fn main() {
///   lustre.simple(init, update, view)
///   |> lustre.start("#root", Nil)
/// }
///
/// fn view(model) {
///   let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Persistent)
///   let options = sketch_lustre.node()
///   use <- skech_lustre.render(options, stylesheet)
///   html.div([], [])
/// }
/// ```
pub fn render(
  options: Options,
  stylesheet: sketch.StyleSheet,
  view: fn() -> element.Element(msg),
) -> el.Element(msg) {
  let stylesheet = mutable.wrap(stylesheet)
  let node = view()
  let #(result, node) = element.unstyled(mutable.get(stylesheet), node)
  let content = sketch.render(result)
  mutable.set(stylesheet, result)
  render_stylesheet(content, node, to_lustre_stylesheet(options))
}

@target(erlang)
fn to_lustre_stylesheet(_options: Options) -> StyleSheet {
  NodeStyleSheet
}

@target(javascript)
fn to_lustre_stylesheet(options: Options) -> StyleSheet {
  let Options(options) = options
  case options {
    Node -> NodeStyleSheet
    Document -> CssStyleSheet(stylesheet.create(stylesheet.Document))
    Shadow(r) -> CssStyleSheet(stylesheet.create(stylesheet.ShadowRoot(r)))
  }
}

fn render_stylesheet(
  content: String,
  node: el.Element(b),
  stylesheet: StyleSheet,
) -> el.Element(b) {
  case stylesheet {
    NodeStyleSheet -> {
      case node {
        vdom.Element(_, _, "lustre-fragment", _, children, _, _) -> {
          el.fragment([el.element("style", [], [el.text(content)]), ..children])
        }
        _ -> el.fragment([el.element("style", [], [el.text(content)]), node])
      }
    }
    CssStyleSheet(stylesheet) -> {
      stylesheet.replace(content, stylesheet)
      node
    }
  }
}

fn contains_head(el: el.Element(a)) -> Bool {
  case el {
    vdom.Element(_, _, "head", _, _, _, _) -> True
    vdom.Element(_, _, _, _, children, _, _) ->
      list.fold(children, False, fn(acc, val) { acc || contains_head(val) })
    _ -> False
  }
}

fn put_in_head(el: el.Element(a), content: String) -> el.Element(a) {
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
/// Can only be used on BEAM as of now.
pub fn ssr(
  el: element.Element(a),
  stylesheet: sketch.StyleSheet,
) -> el.Element(a) {
  let #(stylesheet, el) = element.unstyled(stylesheet, el)
  let stylesheet = sketch.render(stylesheet)
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
pub fn shadow(root: Dynamic) -> Options {
  Options(stylesheet: Shadow(root:))
}
