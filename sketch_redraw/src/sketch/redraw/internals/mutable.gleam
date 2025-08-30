/// `Mutable` wraps any data, and allow to modify it later.
/// `Mutable` should _never_ be used outside of Sketch Redraw,
/// as it breaks the immutability of Gleam.
pub type Mutable(a)

/// Creates a `Mutable` element, without modifying it at first.
@external(javascript, "./mutable.ffi.mjs", "from")
pub fn from(mut: a) -> Mutable(a)

/// Changes the content inside the `Mutable` element. The `Mutable` element
/// remains the same, while _the value inside_ changes.
@external(javascript, "./mutable.ffi.mjs", "set")
pub fn set(mut: Mutable(a), value: a) -> Mutable(a)

/// Get the current value in the `Mutable` value.
@external(javascript, "./mutable.ffi.mjs", "get")
pub fn get(mut: Mutable(a)) -> a
