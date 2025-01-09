import gleam/bool
import gleam/list
import gleam/result
import gleam/string
import simplifile
import snag

pub fn cwd() {
  simplifile.current_directory()
  |> snag.map_error(string.inspect)
  |> snag.context("Impossible to get current directory")
}

/// Assumes dir is a directory. Should be checked before calling the function.
/// Returns a list of file paths.
pub fn readdir(
  dir: String,
  recursive recursive: Bool,
) -> snag.Result(List(String)) {
  use content <- result.map(read_directory(dir))
  let dir = dir <> "/"
  let content = list.map(content, string.append(dir, _))
  use <- bool.guard(when: !recursive, return: content)
  list.flatten({
    use path <- list.filter_map(content)
    use is_dir <- result.try(is_directory(path))
    use <- bool.guard(when: is_dir, return: readdir(path, recursive:))
    Ok([path])
  })
}

pub fn read_file(path: String) -> snag.Result(String) {
  simplifile.read(path)
  |> snag.map_error(string.inspect)
  |> snag.context("Impossible to read file")
  |> snag.context("file: " <> path)
}

pub fn write_file(path: String, content: String) -> snag.Result(Nil) {
  simplifile.write(path, content)
  |> snag.map_error(string.inspect)
  |> snag.context("Impossible to write file")
  |> snag.context("file: " <> path)
}

pub fn mkdir(dir: String, recursive recursive: Bool) -> snag.Result(Nil) {
  case recursive {
    True -> simplifile.create_directory_all(dir)
    False -> simplifile.create_directory(dir)
  }
  |> snag.map_error(string.inspect)
  |> snag.context("Impossible to create directory")
  |> snag.context("dir: " <> dir)
}

fn read_directory(dir: String) {
  simplifile.read_directory(dir)
  |> snag.map_error(string.inspect)
  |> snag.context("Impossible to read the directory")
  |> snag.context("dir: " <> dir)
}

fn is_directory(dir: String) {
  simplifile.is_directory(dir)
  |> snag.map_error(string.inspect)
  |> snag.context("Impossible to test the directory")
  |> snag.context("dir: " <> dir)
}
