// TODO: docs
pub type TypeOf {
  UndefinedType
  ObjectType
  BooleanType
  NumberType
  BigIntType
  StringType
  SymbolType
  FunctionType
}

pub type Symbol

@external(javascript, "../ffi.mjs", "type_of")
pub fn type_of(a: value) -> TypeOf

@external(javascript, "../ffi.mjs", "get_symbol")
pub fn get_symbol(a: String) -> Symbol

pub type Reference(value)

@external(javascript, "../ffi.mjs", "dereference")
pub fn dereference(a: Reference(a)) -> a

@external(javascript, "../ffi.mjs", "set_reference")
pub fn set_reference(a: Reference(a), b: a) -> a

@external(javascript, "../ffi.mjs", "make_reference")
pub fn make_reference(a: a) -> Reference(a)

// returns the old value
pub fn update_reference(ref: Reference(a), f: fn(a) -> a) -> a {
  let value = dereference(ref)
  set_reference(ref, f(value))
  value
}

@external(javascript, "../ffi.mjs", "reference_equal")
pub fn reference_equal(a: Reference(a), b: Reference(a)) -> Bool
