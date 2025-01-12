## v2.0.0 - 2025-01-12

Sketch CSS underwent a major rewrite. From a quick proof of concept drafted in a
day to illustrate the abilities of Sketch, Sketch CSS is now completely
reimplemented from scratch, with support for modern, up to date Gleam, with
partial supports for "code evaluation".

Sketch CSS now somewhat acts as an "interpreter". Instead of reading the code,
and generating the corresponding CSS, Sketch CSS now traverse the sources, and
execute some parts of the code, like an interpreter would do. This allows to
have an whole vision of the Gleam AST, but also to perform variable
interpretation, light computations, dependency detection, cyclic detection, and
more!

Sketch CSS marks the first real step of Sketch as a tool to generate CSS. New
adventures await, with support for Lustre devtools, Vite, and such!

### Features

- Sketch CSS reads `*_styles.gleam`, `*_css.gleam` & `*_sketch.gleam` files to
  generate CSS source files.
- Sketch CSS configuration can be configured in CLI, with `sketch_css.toml`, or
  directly in `gleam.toml`.
