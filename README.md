# Sketch

Sketch is a small module providing CSS-in-Gleam in its simpler form.
Sketch does not try to add complicated API on top of CSS. If you have CSS
knowledge, you'll feel right at home, with all the niceties offered by
Sketch, i.e. type-checking of sizes and push-to-browser stylesheets of your
classes, as well as SSR support.

Sketch has currently only one run mode: directly in your browser to leverage on
all abilities of the JS runtime.
It also allows you to build two types of CSS classes: dynamic ones, changing
over time, and static ones, compiled once and for all, and reused during the
entire lifetime of the application, just like classic CSS stylesheets.

Sketch is thought to interact nicely with [Lustre](https://hexdocs.pm/lustre/),
but can also be used directly in your vanilla Gleam application or in your
fully-featured application. This should probably only be considered to create
custom framework or to integrate Sketch in your favorite framework, because Sketch
has its own lifecycle to render styles. More informations can be found in the docs.

## Installation

Sketch is published on [Hex](https://hex.pm/packages/sketch). Add it to your
project by using the gleam CLI.

```bash
gleam add sketch
```

## Setup

If you're using Lustre (which is strongly recommended), you can just use the
[`lustre_setup`](https://hexdocs.pm/sketch/sketch.html#lustre_setup) function.

Otherwise, you have to follow the lifecycle of Sketch, and use the three low-level
functions [`create_cache`](https://hexdocs.pm/sketch/sketch.html#create_cache),
[`prepare`](https://hexdocs.pm/sketch/sketch.html#prepare) and [`render`](https://hexdocs.pm/sketch/sketch.html#render).
Create the cache with [`create_cache`](https://hexdocs.pm/sketch/sketch.html#create_cache)
and before every repaint of your frontend, call [`prepare`](https://hexdocs.pm/sketch/sketch.html#prepare).
After the repaint, synchronously, call [`render`](https://hexdocs.pm/sketch/sketch.html#render),
and let the magic happen in your browser. Heads up in the docs for more details.

## Example with Lustre

```gleam
import sketch
import sketch/options as sketch_options
import gleam/int
import lustre
import lustre/element.{text}
import lustre/element/html.{div, button, p}
import lustre/event.{on_click}

pub fn main() {
  let assert Ok(render) = sketch.setup(sketch_options.browser())
  let app = lustre.simple(init, update, render(view))
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

fn init(_flags) {
  0
}

type Msg {
  Incr
  Decr
}

fn update(model, msg) {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
  }
}

fn main_class() {
  sketch.class([
    sketch.background("red"),
    sketch.display("flex"),
    sketch.flex_direction("row"),
    sketch.gap(px(12)),
    sketch.padding(px(12)),
    sketch.hover([sketch.background("yellow")]),
    sketch.media(media.max_width(px(450)), [
      sketch.background("purple"),
      sketch.hover([sketch.background("white")]),
    ]),
  ])
  |> sketch.to_lustre()
}

fn color_class(model: Model) {
  let back = case model % 3 {
    0 -> "blue"
    _ -> "green"
  }
  let id = "color-" <> back
  sketch.dynamic(id, [sketch.background(back)])
  |> sketch.to_lustre()
}

fn view(model) {
  let count = int.to_string(model)

  div([main_class()], [
    button([on_click(Incr)], [text(" + ")]),
    p([color_class()], [text(count)]),
    button([on_click(Decr)], [text(" - ")])
  ])
}
```

## Compiling static classes

Sketch exposes a single function [`class`](https://hexdocs.pm/sketch/sketch.html#class)
allowing you to build your class. The first time your function is called, the
corresponding styles will be compiled into CSS rules, and pushed in your browser
or your SSR stylesheet. Every time you'll call the function in the future, no
computation will be done, the class name will be returned, thanks to memoization.

```gleam
import sketch

fn my_class() -> String {
  sketch.class([
    sketch.display("flex"),
    sketch.flex_direction("column"),
  ])
  |> sketch.to_class_name()
}
```

## Compiling dynamic classes

Sketch exposes another function [`dynamic`](https://hexdocs.pm/sketch/sketch.html#dynamic)
allowing you to build a dynamic class, changing over time. Each time the function
is called, the properties in the declaration will be compiled into CSS, the previous
class will be wiped from the browser, and the new one will pushed.

An ID *should be provided* at the moment, due to a limitation of the runtime, and
the limitations of the compilation. Until a workaround is found, be careful to
provide a unique id for your dynamic class, to avoid overlap with other classes.

```gleam
import gleam/bool
import sketch

fn my_dynamic_class(is_column: Bool) -> String {
  // id is unfortunately required at the moment.
  let id = "column-" <> bool.to_string(is_column)
  sketch.dynamic(id, [
    sketch.display("flex"),
    case is_column {
      True -> sketch.flex_direction("column")
      False -> sketch.flex_direction("row")
    }
  ])
  |> sketch.to_class_name()
}
```

## Using media queries and pseudo-selectors

Because we're in CSS-in-Gleam, we can leverage on the full CSS power,
contrarily to inline styling. This mean we can use media queries and pseudo-selectors!
You only need to call the proper functions, and sketch will take care of the rest.

```gleam
import sketch
import sketch/media
import sketch/size.{px}

fn my_class() {
  sketch.class([
    sketch.display("flex"),
    sketch.flex_direction("row"),
    sketch.background("red"),
    sketch.hover([
      sketch.background("blue"),
    ]),
    sketch.media(media.max_width(px(320)), [
      sketch.flex_direction("column"),
      sketch.hover([
        sketch.background("green"),
      ]),
    ]),
  ])
  |> sketch.to_lustre()
}
```

The example above will be compiled to the following CSS.

```css
.css-001 {
  display: flex;
  flex-direction: row;
  background: red;
}

.css-001:hover {
  background: blue;
}

@media (max-width: 320px) {
  .css-001 {
    flex-direction: column;
  }

  .css-001:hover {
    background: green;
  }
}
```

## Usage with Lustre — Details

[Lustre](https://hexdocs.pm/lustre/) is the recommended framework for frontend
development in Gleam. Sketch tries to simplify as much the development with Lustre.
That's why Sketch exposes a [`lustre_setup`](https://hexdocs.pm/sketch/sketch.html#lustre_setup)
function. This function creates a cache, and returns a middleware for the view function.
It comes as a "hook" (lustre does not offcially supports hooks right now): it setups
the cache before the view, and render the stylesheet after the view has executed.
It tries to be side-effect free in the `view` in order to have a predictable render
in Lustre, and stick with the Elm architecture mindset.

Once setuped, you can use classes in your Lustre views: [`to_lustre()`](https://hexdocs.pm/sketch/sketch.html#to_lustre).
Just use it in place of [`to_class_name()`](https://hexdocs.pm/sketch/sketch.html#to_class_name)
to get a Lustre attribute and use it in your views.

```gleam
import sketch
import lustre/element/html

// With a pipeline.
fn my_view() {
  [sketch.background("red")]
  |> sketch.class()
  |> sketch.to_lustre()
  |> list.repeat(1)
  |> html.div(_, [])
}

// With a dynamic class.
fn my_other_view(model: Bool) {
  let color = case model {
    True -> "red"
    False -> "blue"
  }
  html.div(
    [sketch.to_lustre(sketch.dynamic([sketch.background(color)]))],
    [],
  )
}
```

## Some opinions on properties

A lot of properties are accessible directly through the `sketch` package.
But with time, some could be added, and new features for existing features
can appear. That's why sketch will never try to be on your way: at any time
you can access [`property()`](https://hexdocs.pm/sketch/sketch.html#property),
which allows you to push any arbitrary property in a class. Another thing is ç
that sketch will always let you access raw, low-level properties. If you're
trying to use something like `sketch.width("auto")` and the property does not
support String, look for a variant with an underscore (`_`), it should fullfill
your needs, like `sketch.width_("auto")`!
In case something is missing or a property does not have its underscore
alternative, [open an issue — or better, a PR — on the repo!](https://github.com/ghivert/sketch)
