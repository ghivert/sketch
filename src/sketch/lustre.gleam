import gleam/option.{None, Some}
import lustre/element
import lustre/element/html
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
      None -> el
      Some(content) -> html.div([], [html.style([], content), el])
    }
  }
}
