import gleam/bool
import gleam/list
import gleam/result
import gleam/string
import simplifile
import snag

/// Keep the dirname of a filepath. Fails if path does not exist.
/// Returns the original path if path is not a filepath.
///
/// ```gleam
/// let filepath = "/directory/file.txt"
/// dirname(filepath) == "/directory"
/// ```
pub fn dirname(path: String) -> snag.Result(String) {
  use is_file <- result.map({
    simplifile.is_file(path)
    |> snag.map_error(string.inspect)
    |> snag.context("Impossible to detect file")
  })
  use <- bool.guard(when: !is_file, return: path)
  path
  |> string.split("/")
  |> list.reverse
  |> list.drop(1)
  |> list.reverse
  |> string.join("/")
}

/// Join path segments. Remove extraneous `/`.
pub fn join(path: String, segment: String) -> String {
  [path, segment]
  |> string.join(with: "/")
  |> string.replace(each: "//", with: "/")
}
