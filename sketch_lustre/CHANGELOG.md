## v3.1.0 - 2025-06-04

Sketch Lustre adds a new function, `construct`, to let you inject initial
styling in the stylesheet. Most of the time, so initial style should be
injected, whether to style `<body>`, add some keyframe, some font-faces, or
anything else. Before any use of the stylsheet, `construct` allows you to set
all those things you need!

You can use it to build something like `normalize.css`, but also to build your
very own stylesheet, tailored to your needs!

An example :

```gleam
import lustre
import lustre/element/html
import sketch
import sketch/css
import sketch/css/length.{percent, px}
import sketch/lustre as sl

pub fn main() {
  let assert Ok(stylesheet) = setup_sketch_lustre()
  let view = view(_, stylesheet)
  lustre.application(init, view, update)
  |> lustre.start("#root", Nil)
}

/// Inject default styling in the initial stylesheet.
fn setup_sketch_lustre() {
  use stylesheet <- sl.construct
  stylesheet
  |> sketch.global(css.global("body", [css.margin(px(0))]))
  |> sketch.global(css.global("html", [css.height(percent(100))]))
  // Add any initial style you want here, `sketch.at_rule`, etc.
}

fn view(model: Model, stylesheet: sketch.StyleSheet) {
  use <- sl.render(stylesheet, in: [sl.node()])
  html.div([], [])
}
```

In a nutshell : if you need to instanciate a quick and easy stylesheet, use
`setup()`, if you need to customize your stylesheet, use `construct()`!

## v3.0.0 - 2025-05-09

Sketch Lustre Experimental is now official, and become the official way to use
Sketch with Lustre! Say goodbye to wrapper type, and say hello to pure, native
Lustre `Element`!

Sketch Lustre now uses the new approach to hide the `sketch.StyleSheet` during
`view` creation, to generate directly `lustre/element.Element`.
`sketch/lustre/element.Element` is now gone, meaning Sketch Lustre becomes
easily interoperable with the rest of the Lustre ecosystem! Sketch Lustre marks
a new step for Sketch & Lustre, closing the gap between Lustre & Sketch!

### Breaking Changes

- `sketch/lustre/element.Element` is now gone, and every
  `sketch/lustre/element/html` function generate `lustre/element.Element`
  directly.
- `sketch/lustre.setup` does not accept a `sketch.StyleSheet` anymore, but now
  generates the corresponding persistent stylesheet on its own. This ensures no
  user will choose the wrong stylesheet strategy.

### Improvements

- `sketch/lustre` now exposes `teardown`, that let you get rid of a
  `sketch.StyleSheet`. That ensures no memory leak will happen in applications.
- To mimic Lustre, `sketch_lustre` exposes a new module
  `sketch/lustre/element/keyed`, that exposes the same functions as
  `lustre/element/keyed`, with an additional class when needed.

## v2.0.0 - 2025-01-12

v2.0.0 marks a breaking change with the new Sketch release (i.e. v4.0.0). Sketch
Lustre now uses a new, more idiomatic `render` and `ssr` API, and simplifies as
much a possible the exposed API.

Sketch Lustre has been one of the principal package used in combination with
Lustre. Thanks for your investment!

### Improvements

- Adapt to new Sketch API v4.0.0. `Cache` has been renamed to `StyleSheet`, and
  as such, can cause some inconsistencies bugs with old code.
- Use a new middleware-render API. The old `compose` function is now not
  supported anymore.
- Remove the old `Options` in favour of `Container`.
- Shadow root management has been changed from `plinth` to simple `Dynamic`.
  This can't type-check it, but it avoid having to depend on `plinth` in
  production, to let every user handle `ShadowRoot` as they want.
- Every HMTL element now has MDN Reference links & fragment of description to
  explain how to use them.

## v1.0.3 - 2024-11-06

- Conform to Lustre v4.6.0. Lustre v4.6.0 includes a fragment change internally,
  which is breaking Sketch Lustre. v1.0.3 adapts to that change.

## v1.0.2 - 2024-10-13

- Use correct SVG namespace for SVG elements. SVG elements were previously
  generated as HTML elements. They're now generated as
  `http://www.w3.org/2000/svg` namespaced elements.

## v1.0.1 - 2024-09-17

- Fix a bug where `fold_right` was producing a stack overflow on some JavaScript
  runtimes.

## v1.0.0 - 2024-08-05

Initial release of Sketch Lustre! That first version marks the first separation
of Sketch from a single, huge package, to a core package and companion packages,
specialized for various use cases.

Sketch Lustre includes a complete framework to make Sketch interacts with
Lustre, and provides an easy way to style any HTML node.
