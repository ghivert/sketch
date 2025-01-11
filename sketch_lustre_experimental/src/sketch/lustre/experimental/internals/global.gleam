import sketch

@external(javascript, "../../../../sketch_magic.ffi.mjs", "setStyleSheet")
pub fn set_stylesheet(
  stylesheet: sketch.StyleSheet,
) -> Result(sketch.StyleSheet, Nil) {
  Ok(stylesheet)
}

@external(javascript, "../../../../sketch_magic.ffi.mjs", "getStyleSheet")
pub fn get_stylesheet() -> Result(sketch.StyleSheet, Nil) {
  Error(Nil)
}
