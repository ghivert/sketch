import gleam/list
import gleam/result
import lustre/element.{type Element}
import lustre/element/html
import lustre/internals/vdom
import sketch.{type Cache}
import sketch/options.{type Options}

/// setup is a shortcut for `sketch.create_cache`.
/// It simplify usage for lustre.
pub fn setup(options: Options) {
  sketch.create_cache(options)
}

@target(javascript)
/// Wrap the view function in lustre. Be careful, on BEAM, sketch will add an
/// additional `div` at the root of the HTML tree, to inject the styles in the app.
/// This should have no impact on your app.
pub fn compose(view: fn(model) -> element, cache: Cache) {
  fn(model) {
    sketch.prepare(cache)
    let el = view(model)
    sketch.render(cache)
    el
  }
}

@target(erlang)
pub fn compose(view: fn(model) -> element.Element(msg), cache: Cache) {
  fn(model) {
    sketch.prepare(cache)
    let el = view(model)
    let content = sketch.render(cache)
    case content {
      Error(_) -> el
      Ok(content) -> html.div([], [html.style([], content), el])
    }
  }
}

fn contains_head(el: Element(a)) {
  case el {
    vdom.Element(_, _, "head", _, _, _, _) -> True
    vdom.Element(_, _, _, _, children, _, _) ->
      list.fold(children, False, fn(acc, val) { acc || contains_head(val) })
    // vdom.Fragment(elements, _) ->
    //   list.fold(elements, False, fn(acc, val) { acc || contains_head(val) })
    _ -> False
  }
}

fn put_in_head(el: Element(a), content: String) {
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
pub fn ssr(el: Element(a), cache: Cache) {
  cache
  |> sketch.render()
  |> result.map(fn(content) {
    case contains_head(el) {
      True -> put_in_head(el, content)
      False -> html.div([], [html.style([], content), el])
    }
  })
  |> result.unwrap(el)
}
