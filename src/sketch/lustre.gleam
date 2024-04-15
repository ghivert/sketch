import sketch.{type Cache}
import sketch/options.{type Options}

/// setup is a shortcut for `sketch.create_cache`.
/// It simplify usage for lustre.
pub fn setup(options: Options) {
  sketch.create_cache(options)
}

/// Wrap the view function in lustre.
pub fn compose(view: fn(model) -> element, cache: Cache) {
  fn(model) {
    sketch.prepare(cache)
    let el = view(model)
    sketch.render(cache)
    el
  }
}
