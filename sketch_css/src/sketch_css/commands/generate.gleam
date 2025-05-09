import gleam/io
import gleam/option.{type Option}
import gleam/result
import glint
import sketch_css/generate
import sketch_css/utils
import snag

type Flag =
  Option(String)

pub fn css() -> glint.Command(Nil) {
  use src, dst, interface <- run_glint()
  let assert Ok(directories) = utils.directories(src, dst, interface)
  io.println("Compiling Gleam styles files in " <> directories.src)
  io.println("Writing CSS files to " <> directories.dst)
  io.println("Writing interfaces files to " <> directories.interface)
  generate.stylesheets(directories:)
  |> result.map_error(snag.line_print)
  |> result.map_error(io.println)
  |> result.map(fn(_) {
    io.println("=========")
    io.println("Done!")
  })
  |> result.unwrap_both
}

fn run_glint(continuation: fn(Flag, Flag, Flag) -> Nil) {
  use <- glint.command_help("Generate CSS for your gleam_styles.gleam files!")
  use dst <- glint.flag(dst_flag())
  use src <- glint.flag(src_flag())
  use interface <- glint.flag(interface_flag())
  use _, _, flags <- glint.command
  let dst = dst(flags) |> option.from_result
  let src = src(flags) |> option.from_result
  let interface = interface(flags) |> option.from_result
  continuation(src, dst, interface)
}

fn dst_flag() {
  let msg = "Define the directory in which styles should be output."
  glint.string_flag("dest")
  |> glint.flag_help(msg)
}

fn src_flag() {
  let msg = "Define the directory in which styles should be read."
  glint.string_flag("src")
  |> glint.flag_help(msg)
}

fn interface_flag() {
  let msg = "Define the directory in which interfaces should be output."
  glint.string_flag("interface")
  |> glint.flag_help(msg)
}
