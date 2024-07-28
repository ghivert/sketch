pub opaque type Mutable(a) {
  Mutable(value: a)
}

@external(javascript, "../../sketch_lustre.ffi.mjs", "wrap")
pub fn wrap(value: a) -> Mutable(a) {
  Mutable(value: value)
}

@external(javascript, "../../sketch_lustre.ffi.mjs", "set")
pub fn set(variable: Mutable(a), value: a) -> Mutable(a) {
  Mutable(value: value)
}

@external(javascript, "../../sketch_lustre.ffi.mjs", "get")
pub fn get(variable: Mutable(a)) -> a {
  variable.value
}
