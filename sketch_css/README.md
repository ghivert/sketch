# Sketch CSS

> In case you're here and don't know Sketch, take a look at
> [`sketch` package](https://hexdocs.pm/sketch)!

> This readme is a carbon-copy of the Sketch CSS section in `sketch` readme.

Sketch CSS is a tool to generate CSS from Sketch Class definitions. Because pure
CSS generation is straightforward, `sketch_css` does not need a cache to
generate correct CSS files. Instead, `sketch_css` ships with a CLI tool, able to
read your Gleam styles files, and output corresponding your CSS automagically,
while providing an abstraction layer written in Gleam, to make sure you're using
the right classes! It's an other way to leverage Sketch core and enjoy the
styling in Gleam, while taking advantage of all the static CSS power!

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
