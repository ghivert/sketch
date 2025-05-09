import sketch

@external(erlang, "sketch_global_ffi", "set_stylesheet")
@external(javascript, "./global.ffi.mjs", "setStyleSheet")
pub fn set_stylesheet(
  stylesheet: sketch.StyleSheet,
) -> Result(sketch.StyleSheet, Nil)

@external(erlang, "sketch_global_ffi", "teardown_stylesheet")
@external(javascript, "./global.ffi.mjs", "teardownStyleSheet")
pub fn teardown_stylesheet(stylesheet: sketch.StyleSheet) -> Result(Nil, Nil)

@external(erlang, "sketch_global_ffi", "get_stylesheet")
@external(javascript, "./global.ffi.mjs", "getStyleSheet")
pub fn get_stylesheet() -> Result(sketch.StyleSheet, Nil)

@external(erlang, "sketch_global_ffi", "set_current_stylesheet")
@external(javascript, "./global.ffi.mjs", "setCurrentStylesheet")
pub fn set_current_stylesheet(
  stylesheet: sketch.StyleSheet,
) -> Result(sketch.StyleSheet, Nil)

@external(erlang, "sketch_global_ffi", "dismiss_current_stylesheet")
@external(javascript, "./global.ffi.mjs", "dismissCurrentStylesheet")
pub fn dismiss_current_stylesheet() -> Result(Nil, Nil)
