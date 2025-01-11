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
  // Initialise the cache. Two strategies can be used. Ephemeral caches are designed as throw-away caches.
  let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Ephemeral)
  // Generate the partial view function, compatible with Lustre's runtime.
  lustre.simple(init, update, view(_, stylesheet))
  // And voilà!
  |> lustre.start("#app", Nil)
}

fn view(model, stylesheet) {
  // Add the sketch CSS generation "view middleware".
  use <- sketch_lustre.render(stylesheet, [sketch_lustre.node()])
  // Run your actual view function.
  my_view(model)
}
```

## Usage

`sketch_lustre` exposes two modules to help you build your site, similarly to
Lustre: `sketch/lustre/element` and `sketch/lustre/element/html`. The first one
let you use raw element generation and exposes the Sketch Lustre `Element(msg)`
type, that can be used (almost) interchangeably with Lustre, and element
helpers, i.e. `element`, `fragment`, or even `keyed`.

Because a `sketch_lustre` view function expects an
`sketch/lustre/element.Element(msg)` to paint, you should now write all your
view functions to return Sketch elements. All Sketch elements can be
instanciated with `element`, or with the corresponding
`sketch/lustre/element/html.element`. An element accepts the same thing as a
Lustre element, but includes a `sketch.Class` value as first argument. That
class will be applied to the final generated element.

NB: all elements can be generated using the correct function, or using its
"underscored" version. In the second case, Sketch Lustre behaves _exactly_ like
Lustre, and will not add another class. This is helpful when you want to use a
simple node, without any class linked on it.

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

### On Sketch Lustre Element

A Sketch `Element(msg)` is extremely similar to a Lustre `Element(msg)`,
excepted it carries styles information on top. Going from a
`sketch/lustre/element.Element(msg)` to a `lustre/element.Element(msg)` is
straightforward, by using `sketch/lustre/element.unstyled`. The opposite (going
from a Lustre element to a Sketch Lustre element) is also possible by using
`sketch/lustre/element.styled`!

### Sketch Lustre Experimental

Because sometimes you may want to avoid the `Element(msg)` overhead, you can try
the experimental Sketch Lustre runtime, `sketch_lustre_experimental`. That
runtime works in the same way, excepts it does not implements its own `Element`
type on top of Lustre's `Element`. Most of the time, you should not see any
differences. Keep in mind that it can bug though, as it's still experimental. If
you try to use it, please, report any bugs you can find.

### Usage with Shadow DOM

In browser, Sketch can work with a Shadow DOM, in order to hide the compiled
styles from the rest of the application. With a proper shadow root (represented
as a `Dynamic` in Gleam), you can use
[`sketch/lustre.shadow()`](https://hexdocs.pm/sketch_lustre/sketch/lustre.html#shadow)
to render a stylesheet in the shadow root directly. In the same way you can
initialize the cache to render in document or in a `style` node, you can use a
shadow root to paint styles in your application!
