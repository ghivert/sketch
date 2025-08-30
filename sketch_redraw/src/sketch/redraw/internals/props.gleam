/// Add `field_name` to `value` of the props `props`.
/// It will generate a new Object, and will not perform side-effect.
@external(javascript, "./props.ffi.mjs", "append")
pub fn append(props: props, field_name: String, value: value) -> props
