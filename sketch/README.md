# Sketch

Sketch is a module providing CSS-in-Gleam in its simpler form. Sketch does not
try to add complicated API on top of CSS. If you have CSS knowledge, you'll feel
right at home, with all the niceties offered by Sketch, i.e. type-checking of
sizes and push-to-browser stylesheets of your classes, as well as SSR support.

Sketch supports both runtime of Gleam, and will let you write your CSS without
over-thinking about it. Let Sketch handle the hard task for you of CSS caching,
generation and pushing it in the browser. Sketch do the right choices for you,
to maximise performance in the browser and on BEAM.

## Distributions

Sketch is thought as bare package, built as a foundation for every CSS packages
that want to leverage on it. In the Sketch package, you'll find all CSS
properties accessible, as well as low level generation functions, to go from
Sketch to CSS. Sketch package is also made for framework developers, to provide
a common basement, reusable across the entire Gleam ecosystem, letting users
reuse their knowledge no matter what they are coding.

Sketch already supports two compilation target:
[Lustre](https://hexdocs.pm/lustre/), with
[`sketch_lustre`](https://hexdocs.pm/sketch_lustre/), and pure CSS generation à
la CSS Modules, with [`sketch_css`](https://hexdocs.pm/sketch_css/). As a user,
you want to grab one of those package, and start building! Both targets can be
mixed too, to reach whoever you want! For framework authors, let's see you at
[integration part](#integration)!

## Installation

Sketch is published on [Hex](https://hex.pm/packages/sketch). Add it to your
project by using the gleam CLI.

```bash
# For Lustre integration
gleam add sketch sketch_lustre
# For CSS generation
gleam add sketch sketch_css
```

## Core concept

Sketch focuses on the concept of generating CSS in a performant way. To do it,
Sketch needs to use a cache. The cache allows to avoid repeating unneeded
computations, and ensure consistency across repaints. Because the browser likes
static CSS, using a cache make sure the browser will not undergo unneeded
computations to recompute styles at every repaint.

## Sketch Lustre

### Setup

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

### Usage

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
    html.div_([], [h.text(int.to_string(model))]),
  ])
}
```

And you're done! Enjoy your Lustre app, Sketch-enhanced!

### Final notes

#### On Sketch Lustre Element

A Sketch `Element(msg)` is extremely similar to a Lustre `Element(msg)`,
excepted it carries styles information on top. Going from a
`sketch/lustre/element.Element(msg)` to a `lustre/element.Element(msg)` is
straightforward, by using `sketch/lustre/element.unstyled`. The opposite (going
from a Lustre element to a Sketch Lustre element) is also possible by using
`sketch/lustre/element.styled`!

#### Usage with Shadow DOM

In browser, Sketch can work with a Shadow DOM, in order to hide the compiled
styles from the rest of the application. To do it, you can use
[`plinth`](https://github.com/CrowdHailer/plinth). This allows to create a
`ShadowRoot`, to use
[`sketch/options.shadow_root()`](https://hexdocs.pm/sketch/sketch/options.html#shadow_root).
In the same way you can initialize the cache to render in document or in a
`style` node, you can now use a Shadow Root to paint styles in your application!

## Sketch Redraw

### Setup

When you're using Redraw, `sketch_redraw` covers you. `sketch_redraw` exposes
one entrypoint, `sketch/redraw`, containing everything needed to get started.

```gleam
// main.gleam
import redraw
import sketch
import sketch/redraw as sketch_redraw

pub fn main() {
  let root = client.create_root("root")
  client.render(root, redraw.strict_mode([
    // Initialise the cache. Sketch Redraw handles the details for you.
    sr.provider([
      // Here comes your components!
    ])
  ]))
}
```

### Usage

`sketch_redraw` exposes one module to help you build your site, similarly to
redraw: `sketch/redraw/html`. `html` is simply a supercharged component,
accepting a `sketch.Class` as first argument, and applies that style to the
node. Because it's a simple component, `sketch/redraw/html` and `redraw/html`
can be mixed in the same code without issue! Because of that property,
`sketch_redraw` _does not_ expose `text` and `none` function at that time.

```gleam
import redraw/html as h
import sketch
import sketch/redraw/html
import sketch/size.{px}

fn main_style() {
  sketch.class([
    sketch.background("red"),
    sketch.font_size(px(16)),
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

### Final notes

Sketch Redraw tries to integrate nicely with React Devtools! In case you're
seeing something weird, signal the bug!

## Sketch CSS

Because pure CSS generation is straightforward, `sketch_css` does not need a
cache to generate correct CSS files. Instead, `sketch_css` ships with a CLI
tool, able to read your Gleam styles files, and output corresponding your CSS
automagically, while providing an abstraction layer written in Gleam, to make
sure you're using the right classes! It's an other way to leverage on Sketch
core and enjoy the styling in Gleam, while taking advantage of all the static
CSS power!

To run the generator, you have to use the command
`gleam run -m sketch/css generate` at the root of your project. By default,
`sketch_css` will try to read all files named `*_styles.gleam`, `*_css.gleam`
and `*_sketch.gleam` in your `src` folder, no matter where they are. You can put
them at root, nested, or in a folder called `css`, `sketch_css` does not care!
After fetching the styles files, `sketch_css` will output your generated CSS
files in a `styles` folder, at the root of the project. They can then be served
in the way you want. In the same time, `sketch_css` will output Gleam interfaces
in `src/sketch/styles`, matching your styles files, to use in your project!

### Options

Sketch CSS generation has strong defaults, but everything can be customised. Use
the CLI flags to configure what you need. CLI exposes 3 flags:

- `--dest`, accepting a folder, relative to current directory. It defaults to
  `styles`
- `--src`, accepting a folder, relative to current directory. It defaults to
  `src`.
- `--interface`, accepting a folder, relative to current directory. It defaults
  to `src/sketch/styles`.

### A note on generation algorithm

Because a Sketch `Class` can be generated in multiple ways, and with variable,
Sketch CSS takes that into account. Every simple Sketch `Class` will be iso
generated in CSS, but every Sketch `Class` that contains variable will be
generated with the variable taken into account! Sketch CSS being opinionated, it
generates the class, with a CSS variable, letting you update it, override it,
etc.

All `_` are also automatically transformed into `-`, because CSS classes are
most of the time used with dashes, so Sketch CSS follows that convention!

### Example

```gleam
// src/main_styles.gleam
import sketch

fn flexer() {
  sketch.class([
    sketch.display("flex"),
  ])
}

fn flexer_direction(flex_direction: String) {
  sketch.class([
    sketch.compose(flexer()),
    sketch.flex_direction(flex_direction),
  ])
}
```

```css
/* styles/main_styles.css */
.flexer {
  display: flex;
}

.flexer-direction {
  flex-direction: var(--flex-direction);
}
```

```gleam
// src/sketch/styles/main_styles.gleam
pub const flexer = "flexer"

pub const flexer_direction = "flexer flexer-direction"
```

## Sketch general usage

At its core, Sketch relies on `sketch.class`, which let you define a class. A
class is made of CSS properties. All of those can be accessed in `sketch`
module. Build your classes, and use them across your codebase!

## Using media queries and pseudo-selectors

Because we're building CSS, we can leverage on its full power, contrarily to
inline styling. This mean we can use media queries and pseudo-selectors! You
only need to call the proper functions, and Sketch will take care of the rest.

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
}
```

The example above will be compiled to the following CSS.

```css
.my-class {
  display: flex;
  flex-direction: row;
  background: red;
}

.my-class:hover {
  background: blue;
}

@media (max-width: 320px) {
  .my-class {
    flex-direction: column;
  }

  .my-class:hover {
    background: green;
  }
}
```

## Composition

Because we oftentimes need to compose CSS classes, Sketch provides a `compose`
function, acting like CSS Modules `compose` property. This allow you to reuse
CSS properties from another class, without having the burden of copy-pasting the
styles, or having to think on the class names to put in your nodes! Of course,
this remains totally optional. An example:

```gleam
fn button_style() {
  sketch.class([
    sketch.appearance("none"),
    sketch.border("none"),
    sketch.border_radius(px(10)),
    sketch.transition("all .2s"),
  ])
}

fn enabled_button_style() {
  sketch.class([
    sketch.compose(button_style()),
    sketch.background("red"),
    sketch.color("white"),
  ])
}

fn disabled_button_style() {
  sketch.class([
    sketch.compose(button_style()),
    sketch.background("grey"),
    sketch.color("black"),
  ])
}

fn button(disabled) {
  let class = case disabled {
    True -> disabled_button_style()
    False -> enabled_button_style()
  }
  html.button(class, [], [html.text("Yay!")])
}
```

## Some opinions on properties

A lot of properties are accessible directly through the `sketch` package. But
with time, some could be added, and new features for existing properties can
appear. That's why Sketch will never try to be on your way: at any time you can
access [`property()`](https://hexdocs.pm/sketch/sketch.html#property), which
allows you to push any arbitrary property in a class. Another thing is that
Sketch will always let you access raw, low-level properties. If you're trying to
use something like `sketch.width("auto")` and the property does not support
String, look for a variant with an underscore (`_`), it should fullfill your
needs, like `sketch.width_("auto")`! In case something is missing or a property
does not have its underscore alternative,
[open an issue — or better, a PR — on the repo!](https://github.com/ghivert/sketch)

## Integration

> This part is new, and subject to modification. Because nobody integrated
> Sketch in their framework yet, it's hard to write a correct guide, that is
> useful and not redundant. If you're in the case of writing a framework
> binding, please, let's keep in touch directly, and I'll help you integrate
> Sketch. That would be immensely helpful, to write a correct guide after this!
> Meanwhile, you can find necessary pointers below to help you get started by
> yourself!

If you're here, it means you're interested in integrating Sketch in your
framework! What a wonderful idea!

To integrate Sketch in your framework, you have 2 choices:

- run Sketch in your repaint function.
- compiles all Sketch files as static code.

To run Sketch in your repaint function, your only need is to run
`sketch.class_name` on a `sketch.Class`. Let your users write `sketch.Class`,
and then, do the hard work of wiring everything up by calling
`sketch.class_name`. This requires a `sketch.Cache` to run correctly. Take a
look at what is happening in `sketch_lustre` to figure out how everything works.

A nice way is also to precompile everything, like `sketch_css` is doing. Instead
of generating the CSS on-the-fly, which browsers does not really like, you can
precompute everything. By using a Gleam parser, like `glance`, you could compile
everything to plain CSS. This area is subject of exploration, and is the way
`sketch_lustre` tries to follow in some specific environments, like Vite and
Lustre Dev tools. If you're interested in the subject, let's keep in touch!
