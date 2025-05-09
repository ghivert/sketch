import gleam/dynamic.{type Dynamic}
import gleam/function
import gleam/list
import lustre/element as el
import lustre/element/html as h
import lustre/vdom/vnode
import sketch
import sketch/lustre/experimental/internals/css_stylesheet
import sketch/lustre/experimental/internals/global

/// Location to output the generated CSS. Every `render` or `ssr` call expect
/// one or more containers, and dump the generated CSS inside. Containers can
/// be created with [`document()`](#document), [`node()`](#node) or
/// [`shadow()`](#shadow).
pub opaque type Container {
  Document(css_stylesheet: Dynamic)
  Shadow(css_stylesheet: Dynamic)
  Node
}

/// Setup the StyleSheet to use across your application. One stylesheet must be
/// set in your entire application. `setup` should be called once at startup.
/// You can call `setup` any time you want to reset your stylesheet. \
/// Because of performance reasons and race conditions, non-persistent
/// stylesheets will fail to initialize the package. Always use a `Persistent`
/// stylesheet with `sketch_lustre_experimental`.
///
/// ```gleam
/// pub fn main() {
///   let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Persistent)
///   let assert Ok(_) = sketch_lustre.setup(stylesheet)
///   // The following code goes there.
/// }
/// ```
pub fn setup(stylesheet: sketch.StyleSheet) -> Result(sketch.StyleSheet, Nil) {
  global.set_stylesheet(stylesheet)
}

/// Use `render` as a view middleware. Like in Wisp, `sketch_lustre_experimental`
/// adopts the middleware philosophy in Lustre, and allows you to call `render`
/// directly in your view function, by using `use`. No need to wrap your view
/// function.
///
/// ```gleam
/// import lustre
/// import lustre/element
/// import sketch
/// import sketch/lustre/experimental as sketch_lustre
/// import sketch/lustre/experimental/element/html
///
/// pub fn main() {
///   let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Persistent)
///   let assert Ok(_) = sketch_lustre.setup(stylesheet)
///   lustre.simple(init, update, view(_, stylesheet))
///   |> lustre.start("#root", Nil)
/// }
///
/// fn view(model: model, stylesheet: sketch.StyleSheet) -> element.Element(msg) {
///   use <- skech_lustre.render(stylesheet, in: [sketch_lustre.node()])
///   html.div([], [])
/// }
/// ```
pub fn render(
  stylesheet stylesheet: sketch.StyleSheet,
  in outputs: List(Container),
  after view: fn() -> el.Element(msg),
) -> el.Element(msg) {
  let assert Ok(_) = global.set_current_stylesheet(stylesheet)
  let new_view = view()
  case global.get_stylesheet() {
    Error(_) -> new_view
    Ok(stylesheet) -> {
      let content = sketch.render(stylesheet)
      use view, stylesheet <- list.fold(outputs, new_view)
      case stylesheet {
        Node -> el.fragment([h.style([], content), view])
        Document(css_stylesheet:) | Shadow(css_stylesheet:) -> {
          use _ <- function.tap(view)
          css_stylesheet.replace(content, css_stylesheet)
        }
      }
    }
  }
}

/// Use `ssr` as a view middleware. Like in Wisp, `sketch_lustre_experimental`
/// adopts the middleware philosophy in Lustre, and allows you to call `ssr`
/// directly in your view function, by using `use`. No need to wrap your view
/// function.
///
/// ```gleam
/// import gleam/bytes_tree
/// import gleam/http/response
/// import lustre
/// import lustre/element
/// import mist
/// import sketch
/// import sketch/lustre/experimental as sketch_lustre
/// import sketch/lustre/experimental/element/html
/// import wisp
///
/// pub fn main() {
///   let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Persistent)
///   let assert Ok(_) = sketch_lustre.setup(stylesheet)
///   let assert Ok(_) =
///     fn(_) { greet(stylesheet) }
///     |> mist.new()
///     |> mist.port(1234)
///     |> mist.start_http()
///   process.sleep_forever()
/// }
///
/// pub fn greet(stylesheet: sketch.StyleSheet) -> wisp.Response {
///   view(model, stylesheet)
///   |> element.to_document_string
///   |> bytes_tree.to_document_string
///   |> mist.Bytes
///   |> response.set_body(response.new(200), _)
/// }
///
/// fn view(model: model, stylesheet: sketch.StyleSheet) -> element.Element(msg) {
///   use <- skech_lustre.ssr(stylesheet)
///   html.div([], [])
/// }
/// ```
pub fn ssr(view: fn() -> el.Element(a)) -> el.Element(a) {
  let new_view = view()
  case global.get_stylesheet() {
    Error(_) -> new_view
    Ok(stylesheet) -> {
      let content = sketch.render(stylesheet)
      case contains_head(new_view) {
        True -> put_in_head(new_view, content)
        False -> el.fragment([h.style([], content), new_view])
      }
    }
  }
}

@target(javascript)
/// Create a container in the window document. Create a `CSSStyleSheet` and
/// updates the content directly inside. \
/// Because `document` is only accessible in the browser, that function cannot
/// be called on Erlang target.
pub fn document() -> Container {
  let css_stylesheet = css_stylesheet.create(css_stylesheet.Document)
  Document(css_stylesheet:)
}

@target(javascript)
/// Create a container in a Shadow Root. Create a `CSSStyleSheet` and
/// updates the content directly inside. \
/// Because `document` is only accessible in the browser, that function cannot
/// be called on Erlang target.
pub fn shadow(root: Dynamic) -> Container {
  let css_stylesheet = css_stylesheet.create(css_stylesheet.ShadowRoot(root))
  Shadow(css_stylesheet:)
}

/// Create a container directly in the DOM. CSS will be put directly in the
/// elements tree, in a `<style>` tag.
pub fn node() -> Container {
  Node
}

fn contains_head(el: el.Element(a)) -> Bool {
  case el {
    vnode.Element(tag: "head", ..) -> True
    vnode.Element(..) -> {
      use acc, val <- list.fold(el.children, False)
      acc || contains_head(val)
    }
    _ -> False
  }
}

fn put_in_head(el: el.Element(a), content: String) -> el.Element(a) {
  case el {
    vnode.Element(tag: "head", ..) -> {
      vnode.Element(..el, children: {
        let style = h.style([], content)
        [style, ..el.children]
      })
    }

    vnode.Element(tag: "html", ..) -> {
      vnode.Element(..el, children: {
        use child <- list.map(el.children)
        put_in_head(child, content)
      })
    }

    node -> node
  }
}
