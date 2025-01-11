# Sketch

Sketch provides CSS support in Gleam in its simpler — yet complete — form.
Sketch does not add complicated API on top of CSS. If you have CSS knowledge,
you'll feel right at home, with all the niceties offered by Sketch, i.e.
type-checking of dimensions and push-to-browser stylesheets of your classes, as
well as SSR support or CSS files generation.

Sketch supports both runtime of Gleam, and will let you write your CSS without
over-thinking about it. Let Sketch handle the hard task for you of CSS caching,
generation and pushing it in the browser. Sketch do the right choices for you,
to maximise performance in the browser and on BEAM.

Write your styles once, use them anywhere you want.

## Distributions

Sketch is thought as bare package, built as a foundation for every CSS packages
that want to leverage it. In the core package, you'll find all CSS properties
accessible and a way to convert them directly in plain CSS. \
Sketch package is also made for framework developers, to provide a common
basement, reusable across the entire Gleam ecosystem, letting users reuse their
knowledge no matter what they are coding.

Sketch supports officially three compilation target:
[Lustre](https://hexdocs.pm/lustre/), with
[`sketch_lustre`](https://hexdocs.pm/sketch_lustre/),
[Redraw](https://hexdocs.pm/redraw/) with
[`sketch_redraw`](https://hexdocs.pm/sketch_redraw/) and pure CSS generation à
la CSS Modules, with [`sketch_css`](https://hexdocs.pm/sketch_css/). Lustre has
also two versions, with one containing experimental modifications. As a user,
you want to grab one of those package, and start building! All targets can be
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
Sketch uses a virtual stylesheet. That stylesheet allows to avoid repeating
unneeded computations, and ensure consistency across repaints. Because the
browser likes static CSS, using a virtual stylesheet make sure the browser will
not undergo unneeded computations to recompute styles at every repaint.

Almost every CSS has to reside in a class to take effect. Sketch reuse the same
concepts, and ask you to write Sketch classes, and then to pass those classes
around to use them. A class is made of CSS declarations, which are accessible in
the module `sketch/css`. Simply use functions in this module to generate the CSS
you want.

This may seem to add a bit of boilerplate, but Sketch is compatible with every
runtime, and can also generate static CSS. As such, reusing the class
abstraction allows a greater flexibility, without adding too much burden, as
they're still all generated at runtime. Sketch favour explicitness and CSS
generation for every node instead of relying on cascading and inheritance.

## Examples

Want to see examples to jump directly on subject? Take a look at
[e2e folder on GitHub](https://github.com/ghivert/sketch/tree/main/e2e) to see
how it works!

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

### Final notes

#### On Sketch Lustre Element

A Sketch `Element(msg)` is extremely similar to a Lustre `Element(msg)`,
excepted it carries styles information on top. Going from a
`sketch/lustre/element.Element(msg)` to a `lustre/element.Element(msg)` is
straightforward, by using `sketch/lustre/element.unstyled`. The opposite (going
from a Lustre element to a Sketch Lustre element) is also possible by using
`sketch/lustre/element.styled`!

#### Sketch Lustre Experimental

Because sometimes you may want to avoid the `Element(msg)` overhead, you can try
the experimental Sketch Lustre runtime, `sketch_lustre_experimental`. That
runtime works in the same way, excepts it does not implements its own `Element`
type on top of Lustre's `Element`. Most of the time, you should not see any
differences. Keep in mind that it can bug though, as it's still experimental. If
you try to use it, please, report any bugs you can find.

#### Usage with Shadow DOM

In browser, Sketch can work with a Shadow DOM, in order to hide the compiled
styles from the rest of the application. With a proper shadow root (represented
as a `Dynamic` in Gleam), you can use
[`sketch/lustre.shadow()`](https://hexdocs.pm/sketch_lustre/sketch/lustre.html#shadow)
to render a stylesheet in the shadow root directly. In the same way you can
initialize the cache to render in document or in a `style` node, you can use a
shadow root to paint styles in your application!

## Sketch Redraw

### Setup

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

### Usage

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

### Final notes

Sketch Redraw tries to integrate nicely with React Devtools! In case you're
seeing something weird, signal the bug!

## Sketch CSS

Because pure CSS generation is straightforward, `sketch_css` does not need a
cache to generate correct CSS files. Instead, `sketch_css` ships with a CLI
tool, able to read your Gleam styles files, and output corresponding CSS
automagically, while providing an abstraction layer written in Gleam, to make
sure you're using the right classes! It's an other way to leverage Sketch core
and enjoy the styling in Gleam, while taking advantage of all the static CSS
power!

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

Sketch CSS generation has strong defaults, but everything can be customised. To
pass options to Sketch CSS, you have three ways:

- Pass them directly on the CLI. Every option has its equivalent exposed in the
  CLI.
- Write them in a `sketch_css.toml` file, at root of your project, next
  `gleam.toml`.
- Write them directly in `gleam.toml`, under `[sketch_css]` section.

Sketch CSS has 3 distinct options:

- `--dest`, accepting a folder, relative to current directory. It defaults to
  `styles`.
- `--src`, accepting a folder, relative to current directory. It defaults to
  `src`.
- `--interface`, accepting a folder, relative to current directory. It defaults
  to `src/sketch/styles`.

Write directly the folder, path resolution is done with current working
directory as root.

#### Examples

```sh
gleam run -m sketch_css generate --src="src" --dest="styles" --interface="src/sketch/styles"
```

```toml
# sketch_css.toml
src = "src"
dest = "styles"
interface = "src/sketch/styles"
```

```toml
# gleam.toml
name = "name"
version = "1.0.0"

[sketch_css]
src = "src"
dst = "styles"
interface = "src/sketch/styles"

[dependencies]
gleam_stdlib = ">= 0.34.0 and < 2.0.0"
sketch = ">= 4.0.0 and < 5.0.0"
sketch_css = ">= 2.0.0 and < 3.0.0"

[dev-dependencies]
gleeunit = ">= 1.0.0 and < 2.0.0"
```

### A note on generation algorithm

Because a Sketch `Class` can be generated in multiple ways, and with variable,
Sketch CSS takes that into account. Every simple Sketch `Class` will be iso
generated in CSS, but every Sketch `Class` that contains variable will be
generated with the variable taken into account! Sketch CSS being opinionated, it
generates the class, with a CSS variable, letting you update it, override it,
etc.

Sketch CSS also acts as a basic interpreter. It means you can write basic
constants or variables, and they will be taking into account. Be sure to write
classes like you would do in CSS yet: Sketch CSS does not execute your
functions!

### Example

```gleam
// src/main_styles.gleam
import sketch/css

pub fn flexer() {
  let display = "flex"
  css.class([css.display(display)])
}

fn direction(flex_direction: String) {
  css.flex_direction(flex_direction)
}

pub fn flexer_direction(flex_direction: String) {
  css.class([
    css.compose(flexer()),
    direction(flex_direction),
  ])
}
```

```css
/* styles/main_styles.css */
.main_styles-flexer {
  display: flex;
}

.main_styles-flexer_direction {
  display: flex;
  flex-direction: var(--flex-direction);
}
```

```gleam
// src/sketch/styles/main_styles.gleam
pub const flexer = "main_styles-flexer"

pub const flexer_direction = "main_styles-flexer_direction"
```

## Sketch general usage

At its core, Sketch relies on `sketch.class`, which let you define a class. A
class is made of CSS properties. All of those can be accessed in `sketch`
module. Build your classes, and use them across your codebase! But a Sketch
class contains more than CSS properties, it can also contains every piece of
information used to defined a CSS class. This includes media queries,
pseudo-selectors & combinators! This allows to think to your styles in
isolation, without worrying with the global scope.

## Using media queries and pseudo-selectors

Because we're building CSS, we can leverage its full power, contrarily to inline
styling. This mean we can use media queries and pseudo-selectors! You only need
to call the proper functions, and Sketch will take care of the rest.

```gleam
import sketch/css
import sketch/css/length.{px}
import sketch/css/media

fn my_class() {
  css.class([
    css.display("flex"),
    css.flex_direction("row"),
    css.background("red"),
    css.hover([
      css.background("blue"),
    ]),
    css.media(media.max_width(px(320)), [
      css.flex_direction("column"),
      css.hover([
        css.background("green"),
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

## Combinators

Sometimes, you need to modify some CSS of a child, when a parent state changes.
In such cases, you need to use a combinator. Combinators are common in CSS, and
they can be used easily in Sketch. Sketch exposes the 4 main combinators:
`child`, `descendant`, `next_sibling` and `subsequent_sibling`, i.e. `>`, ` `,
`+` and `~`. They have to be used with target classes, and they will be combined
in the resulting CSS!

```gleam
import sketch/css

fn my_class() {
  css.class([
    css.background("red"),
    css.hover([
      css.background("blue"),
      css.child(button_class(), [
        css.background("yellow"),
      ]),
    ]),
    css.child(button_class(), [
      css.background("green"),
    ]),
  ])
}

fn button_class() {
  css.class([
    css.appearance("none"),
    css.background("none"),
    css.border("1px solid black"),
    css.font_family("inherit"),
    css.font_size_("inherit"),
  ])
}
```

Will give the following CSS.

```css
.my_class {
  background: red;
}

.my_class:hover {
  background: blue;
}

.my_class:hover > .button_class {
  background: yellow;
}

.my_class > .button_class {
  background: green;
}

.button_class {
  appearance: none;
  background: none;
  border: 1px solid black;
  font-family: inherit;
  font-size: inherit;
}
```

## Composition

Because we oftentimes need to compose CSS classes, Sketch provides a `compose`
function. This allow you to reuse CSS properties from another class, without
having the burden of copy-pasting the styles, or having to think on the class
names to put in your nodes! Of course, this remains totally optional. An
example:

```gleam
fn button_style() {
  css.class([
    css.appearance("none"),
    css.border("none"),
    css.border_radius(px(10)),
    css.transition("all .2s"),
  ])
}

fn enabled_button_style() {
  css.class([
    css.compose(button_style()),
    css.background("red"),
    css.color("white"),
  ])
}

fn disabled_button_style() {
  css.class([
    css.compose(button_style()),
    css.background("grey"),
    css.color("black"),
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

## Top-level CSS

Sometimes, you need to write CSS directly in stylesheets, at the top-level.
Sketch implements a cherry-picked subset of
[@rules](https://developer.mozilla.org/docs/Web/CSS/At-rule). You can use them
directly on stylesheet, and they will be bundled in your resulting stylesheet!

```gleam
import sketch
import sketch/css

pub fn main() {
  let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Ephemeral)
  let stylesheet = sketch.at_rule(my_keyframe(), stylesheet)
  let content = stylesheet.render(stylesheet)
}

fn my_keyframe() {
  css.keyframes("fade-out", [
    keyframe.from([css.opacity(1.0)]),
    keyframe.at(50, [css.opacity(0.5)]),
    keyframe.to([css.opacity(0.0)]),
  ])
}
```

In the above code, `content` will contains the following CSS.

```css
@keyframes fade-out {
  from {
    opacity: 1;
  }

  50% {
    opacity: 0.5;
  }

  to {
    opacity: 0;
  }
}
```

Similarly, you can use `@font-face` to define your own fonts!

## Some opinions on properties

All standard widely supported properties are accessible directly through the
`sketch/css` package. But with time, some could be added, and new features for
existing properties can appear. Prefixed properties, like `-moz` or `-webkit`,
can also be necessary, when targeting some browsers. That's why Sketch will
never try to be on your way: at any time you can access
[`css.property()`](https://hexdocs.pm/sketch/sketch/css.html#property), which
allows you to push any arbitrary property in a class. Another thing is that
Sketch will always let you access raw, low-level properties. If you're trying to
use something like `sketch.width("auto")` and the property does not support
String, look for a variant with an underscore (`_`), it should fullfill your
needs, like `sketch.width_("auto")`! In case something is missing or a property
does not have its underscore alternative,
[open an issue — or better, a PR — on the repo!](https://github.com/ghivert/sketch)

In the same idea, selectors are plainly supported too! Even if most of them are
already implemented, like `hover`, you could want to define some specific
selectors. In that case, look for
[`css.selector()`](https://hexdocs.pm/sketch/sketch/css.selector)!

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
`css.class_name` on a `css.Class`. Let your users write `css.Class`, and then,
do the hard work of wiring everything up by calling `css.class_name`. This
requires a `sketch.StyleSheet` to run correctly. Take a look at what is
happening in `sketch_lustre` to figure out how everything works.

A nice way is also to precompile everything, like `sketch_css` is doing. Instead
of generating the CSS on-the-fly, which browsers does not really like, you can
precompute everything. By using a Gleam parser, like `glance`, you could compile
everything to plain CSS. This area is subject of exploration, and is the way
`sketch_lustre` tries to follow in some specific environments, like Vite and
Lustre Dev tools. If you're interested in the subject, let's keep in touch!
