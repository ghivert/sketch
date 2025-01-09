import gleam/io
import gleam/option.{type Option}
import glint
import sketch/css/generate
import sketch/css/utils

pub fn css() -> glint.Command(Nil) {
  use src, dst, interface <- run_glint()
  let assert Ok(directories) = utils.directories(src, dst, interface)
  io.println("Compiling Gleam styles files in " <> directories.src)
  io.println("Writing CSS files to " <> directories.dst)
  io.println("Writing interfaces files to " <> directories.interface)
  let _ = generate.stylesheets(directories:)
  io.println("Done!")
  Nil
}

fn run_glint(
  continuation: fn(Option(String), Option(String), Option(String)) -> Nil,
) {
  use <- glint.command_help("Generate CSS for your gleam_styles.gleam files!")
  use dst <- glint.flag(dst_flag())
  use src <- glint.flag(src_flag())
  use interface <- glint.flag(interface_flag())
  use _, _, flags <- glint.command()
  let dst = dst(flags) |> option.from_result
  let src = src(flags) |> option.from_result
  let interface = interface(flags) |> option.from_result
  continuation(src, dst, interface)
}

fn dst_flag() {
  glint.string_flag("dest")
  |> glint.flag_help("Define the directory in which styles should be output.")
}

fn src_flag() {
  glint.string_flag("src")
  |> glint.flag_help(
    "Define the directory in which styles should be read. Default to src.",
  )
}

fn interface_flag() {
  glint.string_flag("interface")
  |> glint.flag_help(
    "Define the directory in which interfaces should be output. Default to src/sketch/styles.",
  )
}
