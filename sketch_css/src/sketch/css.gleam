import argv
import glint
import sketch/css/commands/generate

/// The `main` function is used as an entrypoint for Sketch CSS. That function
/// not meant to be used in your code, but is called when you use `gleam run`
/// from the command line.
///
/// ```
/// gleam run -m sketch/css
/// ```
pub fn main() {
  glint.new()
  |> glint.with_name("Sketch CSS")
  |> glint.pretty_help(glint.default_pretty_help())
  |> glint.add(at: ["generate"], do: generate.css())
  |> glint.run(argv.load().arguments)
}
