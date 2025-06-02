# Sketch Lustre

> In case you're here and don't know Sketch, take a look at
> [`sketch` package](https://hexdocs.pm/sketch)!

> This readme is a carbon-copy of the Sketch Lustre section in `sketch` readme.

## Setup

If you're using Lustre (which is strongly recommended), `sketch_lustre` got you.
`sketch_lustre` exposes one entrypoint, `sketch/lustre`, containing everything
needed to get started.

```gleam
// main.gleam
import lustre
import sketch
import sketch/lustre as sketch_lustre

pub fn main() {
  // Initialise the cache & setup it for use with Lustre. This should be done
  // once before rendering the application. You can initialize one stylesheet
  // for your entire app, or multiple stylesheets if you're running server
  // components (in that case, one stylesheet per client is recommended).
  let assert Ok(stylesheet) = sketch_lustre.setup()
  // Because stylesheets are persistents with sketch_lustre, you can inject
  // classes, keyframes or @rules directly in it.
  sketch.global(stylesheet, css.global("body", [css.margin(px(0))]))
  // Generate the partial view function, compatible with Lustre's runtime.
  lustre.simple(init, update, view(_, stylesheet))
  // And voilà!
  |> lustre.start("#app", Nil)
}

fn view(model, stylesheet) {
  // Add the sketch CSS generation "view middleware". If you don't, your `view`
  // function _will panic_. This behaviour is expected, to make sure you never
  // ship an unstyled application to your customers. A Lustre application made
  // to run with Sketch without StyleSheet setuped will always fail.
  use <- sketch_lustre.render(stylesheet:, in: [sketch_lustre.node()])
  // Run your actual view function.
  my_view(model)
}
```

## Usage

`sketch_lustre` exposes two modules to help you build your site, similarly to
Lustre: `sketch/lustre/element` and `sketch/lustre/element/html`. The first one
let you use raw element generation and element helpers, i.e. `element`,
`fragment`, `map` & `namespaced`.

Every functions from `sketch/lustre/element` or `sketch/lustre/element/html`
returns native `lustre/element.Element` type, but takes an additional argument:
a `sketch.Class` as first argument. You can mix and match Sketch functions
(coming from `sketch/lustre/element`) and classical Lustre functions (coming
from `lustre/element`). There's no difference for the runtime: the former will
simply be a Lustre `Element`, with a class applied on it, while the latter will
be a simple Lustre `Element`.

NB: all elements can be generated using the correct function, or using its
"underscored" version. In the second case, Sketch Lustre behaves _exactly_ like
Lustre, and will not add another class. This is helpful when you want to use a
simple node, without any class linked on it, but you still want to import only
one module, i.e. `import sketch/lustre/element`.

```gleam
import sketch/css
import sketch/css/length.{px}
import sketch/lustre/element
import sketch/lustre/element/html

fn main_style() {
  css.class([
    css.background("red"),
    css.font_size(px(16)),
  ])
}

fn view(model: Int) {
  html.div(main_style(), [], [
    html.div_([], [
      html.text(int.to_string(model)),
    ]),
  ])
}
```

And you're done! Enjoy your Lustre app, Sketch-enhanced!

## Final notes

### Shipping ready-to-use components

Components in Lustre can take various shapes, from simple HMTL functions to
proper, rich components with internal states. In case you want to distribute a
components system built with Lustre (which has numerous advantages, one of them
being that you don't need to ship a `.css` file), it's recommended to not inject
a stylesheet on your own, but rather ask the user to initialize Sketch on their
own. It's usual with Sketch to have one stylesheet, shared among the entire
application. Do not worry about it: browsers tend to favour huge stylesheets.
Indeed, browsers usually deal with static CSS. As such, CSS algorithms are
optimized for classes that do not change. When a class change, lots of changes
should be applied through the DOM, to reflect the new styles. When making sure
no class will ever change in the stylesheet, Sketch ensures the browser will
never have to recompute twice a CSS class!

### Usage with Shadow DOM

In browser, Sketch can work with a Shadow DOM, in order to hide the compiled
styles from the rest of the application. With a proper shadow root (represented
as a `Dynamic` in Gleam), you can use
[`sketch/lustre.shadow()`](https://hexdocs.pm/sketch_lustre/sketch/lustre.html#shadow)
to render a stylesheet in the shadow root directly. In the same way you can
initialize the cache to render in document or in a `style` node, you can use a
shadow root to paint styles in your application!
