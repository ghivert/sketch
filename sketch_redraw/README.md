# Sketch Redraw

> In case you're here and don't know Sketch, take a look at
> [`sketch` package](https://hexdocs.pm/sketch)!

> This readme is a carbon-copy of the Sketch Redraw section in `sketch` readme.

## Setup

When you're using Redraw, `sketch_redraw` covers you. `sketch_redraw` exposes
one entrypoint, `sketch/redraw`, containing everything needed to get started.

```gleam
// main.gleam
import redraw
import sketch/redraw as sketch_redraw

pub fn main() {
  let root = client.create_root("root")
  client.render(root,
    redraw.strict_mode([
      // Initialise the cache. Sketch Redraw handles the details for you.
      sketch_redraw.provider([
        // Here comes your components!
      ])
    ])
  )
}
```

## Usage

`sketch_redraw` exposes one module to help you build your site, similarly to
redraw: `sketch/redraw/dom/html`. `html` is simply a supercharged component,
accepting a `sketch.Class` as first argument, and applies that style to the
node. Because it's a simple component, `sketch/redraw/dom/html` and
`redraw/html` can be mixed in the same code without issue! Because of that
property, `sketch_redraw` _does not_ expose `text` and `none` function at that
time.

```gleam
import redraw/html as h
import sketch/css
import sketch/css/length.{px}
import sketch/redraw/html

fn main_style() {
  css.class([
    css.background("red"),
    css.font_size(px(16)),
  ])
}

fn view(model: Int) {
  html.div(main_style(), [], [
    h.div([], [
      h.text(int.to_string(model))
    ]),
  ])
}
```

And you're done! Enjoy your Redraw app, Sketch-enhanced!

## Final notes

Sketch Redraw tries to integrate nicely with React Devtools! In case you're
seeing something weird, signal the bug!
