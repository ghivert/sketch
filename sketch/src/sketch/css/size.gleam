//// Defines sizes directly with functions, to avoid conversions between int, float
//// and strings.

import gleam/float
import gleam/int
import gleam/string

/// Size defines a CSS Unit. It can be either `px`, `pt`, `vh`, `vw`, `em`,
/// `rem`, `lh`, `rlh`, `ch`, `%`. To instanciate a Size, use the corresponding
/// functions. Every unit exposes two functions: the Int function (like `px(0)`)
/// and the Float version suffixed by an underscore (like `px_(0.0)`).
pub opaque type Size {
  Px(Float)
  Pt(Float)
  Vh(Float)
  Vw(Float)
  Em(Float)
  Rem(Float)
  Lh(Float)
  Rlh(Float)
  Ch(Float)
  Pct(Float)
}

pub fn px(value: Int) -> Size {
  Px(int.to_float(value))
}

pub fn px_(value: Float) -> Size {
  Px(value)
}

pub fn pt(value: Int) -> Size {
  Pt(int.to_float(value))
}

pub fn pt_(value: Float) -> Size {
  Pt(value)
}

pub fn percent(value: Int) -> Size {
  Pct(int.to_float(value))
}

pub fn percent_(value: Float) -> Size {
  Pct(value)
}

pub fn vh(value: Int) -> Size {
  Vh(int.to_float(value))
}

pub fn vh_(value: Float) -> Size {
  Vh(value)
}

pub fn vw(value: Int) -> Size {
  Vw(int.to_float(value))
}

pub fn vw_(value: Float) -> Size {
  Vw(value)
}

pub fn em(value: Float) -> Size {
  Em(value)
}

pub fn rem(value: Float) -> Size {
  Rem(value)
}

pub fn lh(value: Float) -> Size {
  Lh(value)
}

pub fn rlh(value: Float) -> Size {
  Rlh(value)
}

pub fn ch(value: Int) -> Size {
  Ch(int.to_float(value))
}

pub fn ch_(value: Float) -> Size {
  Ch(value)
}

/// Internal function, can be used if you need to go from a Size to a String
/// in case you're building on top of sketch.
@internal
pub fn to_string(size) -> String {
  case size {
    Px(value) -> string.append(float.to_string(value), "px")
    Pt(value) -> string.append(float.to_string(value), "pt")
    Pct(value) -> string.append(float.to_string(value), "%")
    Vh(value) -> string.append(float.to_string(value), "vh")
    Vw(value) -> string.append(float.to_string(value), "vw")
    Em(value) -> string.append(float.to_string(value), "em")
    Rem(value) -> string.append(float.to_string(value), "rem")
    Lh(value) -> string.append(float.to_string(value), "lh")
    Rlh(value) -> string.append(float.to_string(value), "rlh")
    Ch(value) -> string.append(float.to_string(value), "ch")
  }
}
