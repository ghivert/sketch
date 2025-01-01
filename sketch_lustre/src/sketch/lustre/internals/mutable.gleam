pub opaque type Mutable(a) {
  Mutable(value: a)
}

@external(javascript, "../../../mutable.ffi.mjs", "wrap")
pub fn wrap(value: a) -> Mutable(a) {
  Mutable(value: value)
}

@external(javascript, "../../../mutable.ffi.mjs", "set")
pub fn set(_variable: Mutable(a), value: a) -> Mutable(a) {
  Mutable(value: value)
}

@external(javascript, "../../../mutable.ffi.mjs", "get")
pub fn get(variable: Mutable(a)) -> a {
  variable.value
}
