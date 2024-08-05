# Sketch Lustre

> In case you're here and don't know Sketch, take a look at
> [`sketch` package](https://hexdocs.pm/sketch)!

> This readme is a carbon-copy of the Sketch Lustre section in `sketch` readme.

## Setup

Sketch focuses on the concept of generating CSS in a performant way. To do it,
Sketch needs to use a cache. The cache allows to avoid repeating unneeded
computations, and ensure consistency across repaints. Because the browser likes
static CSS, using a cache make sure the browser will not undergo unneeded
computations to recompute styles at every repaint.

If you're using Lustre (which is strongly recommended), `sketch_lustre` got you.
`sketch_lustre` exposes one entrypoint, `sketch/lustre`, containing everything
needed to get started.

```gleam
// main.gleam
import lustre
import sketch
import sketch/lustre as sketch_lustre

pub fn main() {
  // Initialise the cache. Two strategies can be used in browser, only one
  // on server-side.
  let assert Ok(cache) = sketch.cache(strategy: sketch.Ephemeral)
  // Select the output of the generated stylesheet.
  sketch_lustre.node()
  // Add the sketch CSS generation "view middleware".
  |> sketch_lustre.compose(view, cache)
  // Give the new view function to lustre runtime!
  |> lustre.simple(init, update, _)
  // And voilà!
  |> lustre.start("#app", Nil)
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
import sketch
import sketch/lustre/element
import sketch/lustre/element/html
import sketch/size.{px}

fn main_style() {
  sketch.class([
    sketch.background("red"),
    sketch.font_size(px(16)),
  ])
}

fn view(model: Int) {
  html.div(main_style(), [], [
    html.div_([], [h.text(int.to_string(model)]),
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

### Usage with Shadow DOM

In browser, Sketch can work with a Shadow DOM, in order to hide the compiled
styles from the rest of the application. To do it, you can use
[`plinth`](https://github.com/CrowdHailer/plinth). This allows to create a
`ShadowRoot`, to use
[`sketch/options.shadow_root()`](https://hexdocs.pm/sketch/sketch/options.html#shadow_root).
In the same way you can initialize the cache to render in document or in a
`style` node, you can now use a Shadow Root to paint styles in your application!
