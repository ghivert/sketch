import gleam/dynamic.{type Dynamic}
import plinth/browser/shadow.{type ShadowRoot}

pub opaque type Mutable(a) {
  Mutable(value: a)
}

@external(javascript, "../../sketch_lustre.ffi.mjs", "wrap")
pub fn wrap(value: a) -> Mutable(a) {
  Mutable(value: value)
}

@external(javascript, "../../sketch_lustre.ffi.mjs", "set")
pub fn set(_variable: Mutable(a), value: a) -> Mutable(a) {
  Mutable(value: value)
}

@external(javascript, "../../sketch_lustre.ffi.mjs", "get")
pub fn get(variable: Mutable(a)) -> a {
  variable.value
}

@external(javascript, "../../sketch_lustre.ffi.mjs", "createCssStyleSheet")
pub fn create_document_stylesheet() -> Dynamic {
  dynamic.from(0)
}

@external(javascript, "../../sketch_lustre.ffi.mjs", "createCssStyleSheet")
pub fn create_shadow_root_stylesheet(_root: ShadowRoot) -> Dynamic {
  dynamic.from(0)
}

@external(javascript, "../../sketch_lustre.ffi.mjs", "setStylesheet")
pub fn set_stylesheet(_content: String, _stylesheet: Dynamic) -> Nil {
  Nil
}
