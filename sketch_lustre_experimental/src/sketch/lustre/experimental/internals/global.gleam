import sketch

@external(javascript, "../../../../global.ffi.mjs", "setStyleSheet")
pub fn set_stylesheet(
  stylesheet: sketch.StyleSheet,
) -> Result(sketch.StyleSheet, Nil) {
  Ok(stylesheet)
}

@external(javascript, "../../../../global.ffi.mjs", "getStyleSheet")
pub fn get_stylesheet() -> Result(sketch.StyleSheet, Nil) {
  Error(Nil)
}
