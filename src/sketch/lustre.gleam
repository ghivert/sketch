import sketch.{type Cache}
import sketch/options.{type Options}
import lustre.{type App}

@external(javascript, "../sketch.ffi.mjs", "updateLustre")
fn update_lustre(
  app: App(a, b, c),
  view_mapper: fn(fn(model) -> element) -> fn(model) -> element,
) -> App(a, b, c)

pub fn setup(options: Options) {
  sketch.create_cache(options)
}

pub fn wrap(app: App(a, b, c), cache: Cache) {
  use view <- update_lustre(app)
  fn(model) {
    sketch.prepare(cache)
    let el = view(model)
    sketch.render(cache)
    el
  }
}
