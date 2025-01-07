import gleam/bool
import gleam/list
import gleam/result
import gleam/string
import simplifile
import snag

/// Assumes dir is a directory. Should be checked before calling the function.
/// Returns a list of file paths.
pub fn readdir(
  dir: String,
  recursive recursive: Bool,
) -> snag.Result(List(String)) {
  use content <- result.map(simplifile.read_directory(dir) |> directory_error)
  use <- bool.guard(when: !recursive, return: content)
  list.flatten({
    use path <- list.filter_map(content)
    let path = string.join([dir, path], "/")
    use is_dir <- result.try(simplifile.is_directory(path) |> directory_error)
    use <- bool.guard(when: is_dir, return: readdir(path, recursive:))
    Ok([path])
  })
}

pub fn read_file(path: String) -> snag.Result(String) {
  simplifile.read(path)
  |> snag.map_error(string.inspect)
  |> snag.context("Impossible to read file")
}

pub fn write_file(path: String, content: String) -> snag.Result(Nil) {
  simplifile.write(path, content)
  |> snag.map_error(string.inspect)
  |> snag.context("Impossible to write file")
}

pub fn mkdir(dir: String, recursive recursive: Bool) -> snag.Result(Nil) {
  case recursive {
    True -> simplifile.create_directory_all(dir)
    False -> simplifile.create_directory(dir)
  }
  |> snag.map_error(string.inspect)
  |> snag.context("Impossible to create directory")
}

fn directory_error(response: Result(a, b)) -> snag.Result(a) {
  snag.map_error(response, string.inspect)
  |> snag.context("Impossible to read the directory")
}
