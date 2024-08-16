/// Add `field_name` to `value` of the object `object`.
/// It will generate a new Object, and will not perform side-effect.
@external(javascript, "../../../redraw.ffi.mjs", "assign")
pub fn add(object: a, field_name: String, value: b) -> a
