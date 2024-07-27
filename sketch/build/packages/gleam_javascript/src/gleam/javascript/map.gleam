pub type Map(key, value)

@external(javascript, "../../ffi.mjs", "map_new")
pub fn new() -> Map(key, value)

@external(javascript, "../../ffi.mjs", "map_set")
pub fn set(a: Map(key, value), b: key, c: value) -> Map(key, value)

@external(javascript, "../../ffi.mjs", "map_get")
pub fn get(a: Map(key, value), b: key) -> Result(value, Nil)

@external(javascript, "../../ffi.mjs", "map_size")
pub fn size(a: Map(key, value)) -> Int
