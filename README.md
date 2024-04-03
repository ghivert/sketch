# Craft

Craft is a small module providing CSS-in-Gleam in its simpler form.
Craft does not try to add complicated API on top of CSS. If you have CSS
knowledge, you'll feel right at home, with all the niceties offered by
Craft, i.e. type-checking of sizes and push-to-browser stylesheets of your
classes, as well as SSR support.
Craft has currently two run modes: directly in your browser and leverages on
all abilities of the JS runtime, and on backend, to leverages on SSR.
Craft has currently to way to use it: directly in your vanilla Gleam
application or in your fully-featured [Lustre](https://hexdocs.pm/lustre/) application.
Craft allows you to build two types of CSS classes: dynamic ones, changing
over time, and static ones, compiled once and for all, and reused during the
entire lifetime of the application, just like classic CSS stylesheets.

## Compiling static classes

Craft exposes a single function [`class`](#class) allowing you to build your
class. The first time your function is called, the corresponding styles will
be compiled into CSS rules, and pushed in your browser or your SSR stylesheet.
Every time you'll call the function in the future, no computation will be done,
the class name will be returned, thanks to memoization.

```gleam
import craft

fn my_class() -> String {
  craft.class([
    craft.display("flex"),
    craft.flex_direction("column"),
  ])
  |> craft.to_class_name()
}
```

## Compiling dynamic classes

Craft exposes another function [`variable`](#variable) allowing you to build a
dynamic class, changing over time. Each time the function is called, the
properties in the declaration will be compiled into CSS, the previous class
will be wiped from the browser, and the new one will pushed.

```gleam
import craft

fn my_variable_class(is_column: Bool) -> String {
  craft.variable([
    craft.display("flex"),
    case is_column {
      True -> craft.flex_direction("column")
      False -> craft.flex_direction("row")
    }
  ])
  |> craft.to_class_name()
}
```

## Usage with Lustre

[Lustre](https://hexdocs.pm/lustre/) is the main framework for frontend
development in Gleam. Because of this, craft provides a function to directly
use classes in Lustre views: [`to_lustre()`](#to_lustre). Just use it in place
of [`to_class_name()`](#to_class_name) to get a Lustre attribute and use it
in your views.

```gleam
import craft
import lustre/element/html

// With a pipeline.
fn my_view() {
  [craft.background("red")]
  |> craft.class()
  |> craft.to_lustre()
  |> list.repeat(1)
  |> html.div(_, [])
}

// With a variable class.
fn my_other_view(model: Bool) {
  let color = case model {
    True -> "red"
    False -> "blue"
  }
  html.div(
   [craft.to_lustre(craft.variable([craft.background(color)]))],
   [],
  )
}
```

## Using media queries and pseudo-selectors

Because we're in CSS-in-Gleam, we can leverage on the full CSS power,
contrarily to inline styling. This mean we can use media queries and pseudo-selectors!
You only need to call the proper functions, and craft will take care of the rest.

```gleam
import craft
import craft/media
import craft/size.{px}

fn my_class() {
  craft.class([
    craft.display("flex"),
    craft.flex_direction("row"),
    craft.background("red"),
    craft.hover([
      craft.background("blue"),
    ]),
    craft.media(media.max_width(px(320)), [
      craft.flex_direction("column"),
      craft.hover([
        craft.background("green"),
      ]),
    ]),
  ])
  |> craft.to_lustre()
}
```

The example above will be compiled to the following CSS.

```gleam
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

## Some opinions on properties

A lot of properties are accessible directly through the `craft` package.
But with time, some could be added, and new features for existing features
can appear. That's why craft will never try to be on your way: at any time
you can access [`property()`](#property), which allows you to push any
arbitrary property in a class. Another thing is that craft will always let
you access raw, low-level properties. If you're trying to use something like
`craft.width("auto")` and the property does not support String, look for a
variant with an underscore (`_`), it should fullfill your needs, like
`craft.width_("auto")`!
In case something is missing or a property does not have its underscore
alternative, [open an issue — or better, a PR — on the repo!](https://github.com/ghivert/craft)
