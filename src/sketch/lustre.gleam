import sketch.{type Cache}
import sketch/options.{type Options}

pub fn setup(options: Options) {
  sketch.create_cache(options)
}

pub fn wrap(view: fn(model) -> element, cache: Cache) {
  fn(model) {
    sketch.prepare(cache)
    let el = view(model)
    sketch.render(cache)
    el
  }
}
