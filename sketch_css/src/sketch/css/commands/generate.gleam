import gleam/io
import gleam/result
import glint
import sketch/css/generate
import sketch/css/utils

pub fn css() -> glint.Command(Nil) {
  use src, dst, interface <- run_glint()
  let assert Ok(directories) = utils.directories(src, dst, interface)
  io.println("Compiling Gleam styles files in " <> src)
  io.println("Writing CSS files to " <> dst)
  io.println("Writing interfaces files to " <> interface)
  let _ = generate.stylesheets(directories:)
  io.println("Done!")
  Nil
}

fn run_glint(continuation: fn(String, String, String) -> Nil) {
  use <- glint.command_help("Generate CSS for your gleam_styles.gleam files!")
  use dst <- glint.flag(dst_flag())
  use src <- glint.flag(src_flag())
  use interface <- glint.flag(interface_flag())
  use _, _, flags <- glint.command()
  let assert Ok(dst) = dst(flags)
  let assert Ok(src) = src(flags)
  let assert Ok(interface) = interface(flags)
  continuation(src, dst, interface)
}

fn dst_flag() {
  glint.string_flag("dest")
  |> glint.flag_default("styles")
  |> glint.flag_help("Define the directory in which styles should be output.")
}

fn src_flag() {
  glint.string_flag("src")
  |> glint.flag_default("src")
  |> glint.flag_help(
    "Define the directory in which styles should be read. Default to src.",
  )
}

fn interface_flag() {
  glint.string_flag("interface")
  |> glint.flag_default("src/sketch/styles")
  |> glint.flag_help(
    "Define the directory in which interfaces should be output. Default to src/sketch/styles.",
  )
}
