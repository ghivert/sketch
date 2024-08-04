import glance
import gleam/result
import simplifile

pub type Error {
  ApplicationError(message: String)
  SimplifileError(error: simplifile.FileError)
  GlanceError(error: glance.Error)
}

pub fn simplifile(err) {
  result.map_error(err, SimplifileError)
}

pub fn glance(err) {
  result.map_error(err, GlanceError)
}

pub fn not_a_directory(dir: String) {
  let error = ApplicationError(dir <> " is not a directory")
  Error(error)
}
