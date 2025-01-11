# Sketch CSS

> In case you're here and don't know Sketch, take a look at
> [`sketch` package](https://hexdocs.pm/sketch)!

> This readme is a carbon-copy of the Sketch CSS section in `sketch` readme.

Sketch CSS is a tool to generate CSS from Sketch Class definitions. Because pure
CSS generation is straightforward, `sketch_css` does not need a cache to
generate correct CSS files. Instead, `sketch_css` ships with a CLI tool, able to
read your Gleam styles files, and output corresponding CSS automagically, while
providing an abstraction layer written in Gleam, to make sure you're using the
right classes! It's an other way to leverage Sketch core and enjoy the styling
in Gleam, while taking advantage of all the static CSS power!

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
