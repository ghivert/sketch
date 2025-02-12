# Sketch Lustre Experimental

> In case you're here and don't know Sketch, take a look at
> [`sketch` package](https://hexdocs.pm/sketch)!

## Setup

If you're here, it means you're ready to test the future of Sketch Lustre! Be
careful, unexpected things can happen. `sketch_lustre_experimental` exposes one
entrypoint, `sketch/lustre/experimental`, containing everything needed to get
started.

```gleam
// main.gleam
import lustre
import sketch
import sketch/lustre/experimental as sketch_lustre

pub fn main() {
  // Initialise the cache. Two strategies can be used. Ephemeral caches are designed as throw-away caches.
  let assert Ok(_) = sketch.stylesheet(strategy: sketch.Ephemeral)
  // Generate the partial view function, compatible with Lustre's runtime.
  lustre.simple(init, update, view)
  // And voilà!
  |> lustre.start("#app", Nil)
}

fn view(model) {
  // Add the sketch CSS generation "view middleware".
  use <- sketch_lustre.render(in: [sketch_lustre.node()])
  // Run your actual view function.
  my_view(model)
}
```

## Usage

`sketch_lustre_experimental` exposes two modules to help you build your site,
similarly to Lustre: `sketch/lustre/experimental/element` and
`sketch/lustre/experimental/element/html`. The first one let you use raw element
generation and exposes the Lustre `Element(msg)` type. Every time you need
elements helpers, i.e. `element`, `fragment`, or even `keyed`, use directly
`lustre/element` module.

NB: all elements can be generated using the correct function, or using its
"underscored" version. In the second case, Sketch Lustre behaves _exactly_ like
Lustre, and will not add another class. This is helpful when you want to use a
simple node, without any class linked on it.

```gleam
import sketch/css
import sketch/css/length.{px}
import sketch/lustre/experimental/element
import sketch/lustre/experimental/element/html

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

### On Sketch Lustre Experimental Element

A Sketch `Element(msg)` is a Lustre `Element(msg)`, with class name directly
linked on it. You don't need anything to do to get a
`lustre/element.Element(msg)`. This also means you can use any Lustre
`Element(msg)` function you already have.

### Usage with Shadow DOM

In browser, Sketch can work with a Shadow DOM, in order to hide the compiled
styles from the rest of the application. With a proper shadow root (represented
as a `Dynamic` in Gleam), you can use
[`sketch/lustre/experimental.shadow()`](https://hexdocs.pm/sketch_lustre/sketch/lustre.html#shadow)
to render a stylesheet in the shadow root directly. In the same way you can
initialize the cache to render in document or in a `style` node, you can use a
shadow root to paint styles in your application!
