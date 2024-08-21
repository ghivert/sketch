pub type Mutable(a)

@external(javascript, "../../../mutable.ffi.mjs", "wrap")
pub fn wrap(mut: a) -> Mutable(a)

@external(javascript, "../../../mutable.ffi.mjs", "set")
pub fn set(mut: Mutable(a), value: a) -> Mutable(a)

@external(javascript, "../../../mutable.ffi.mjs", "get")
pub fn get(mut: Mutable(a)) -> a
