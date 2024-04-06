let warningDisplayed = false
const lifecycleMissing = 'Sketch lifecycles setup is missing.'
const lifecycleFunctions = `If you're not using lustre, you should use directly the lifecycle functions:
  - create_cache (https://hexdocs.pm/sketch/sketch.html#create_cache)
  - prepare (https://hexdocs.pm/sketch/sketch.html#prepare)
  - render (https://hexdocs.pm/sketch/sketch.html#render)

// main.gleam
// Assuming your framework have a lifecycle, here's a fictive example.

import sketch
import sketch/options as sketch_options

pub fn main() {
  let assert Ok(cache) = sketch.create_cache()
  start_app()
  |> add_before_render(fn () { sketch.prepare(cache) })
  |> add_after_render(fn () { sketch.render(cache) })
}
`
const lustreSetup = `// main.gleam
// If you're using lustre, initialize Sketch in your main().

import sketch
import sketch/options as sketch_options

pub fn main() {
  let assert Ok(render) =
    sketch_options.node()
    |> sketch.lustre_setup()

  let assert Ok(_) =
    lustre.simple(init, update, render(view))
    |> lustre.start("#app", Nil)
}
`

export const warn = {
  setup() {
    if (warningDisplayed) return
    const notRender = 'Meanwhile, styles won’t apply, but will not block your render.'
    const documentation = 'More informations on https://hexdocs.pm/sketch.'
    console.warn(lifecycleMissing)
    console.warn(lustreSetup)
    console.warn(lifecycleFunctions)
    console.warn(notRender)
    console.warn(documentation)
    warningDisplayed = true
  }
}
