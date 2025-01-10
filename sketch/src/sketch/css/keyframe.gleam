import gleam/float
import sketch/internals/cache/cache as style

pub opaque type Keyframe {
  Keyframe(class: style.Class)
}

pub fn from(styles: List(style.Style)) {
  Keyframe(style.named("from", styles))
}

pub fn to(styles: List(style.Style)) {
  Keyframe(style.named("to", styles))
}

pub fn at(percentage: Float, styles: List(style.Style)) {
  Keyframe(style.named(float.to_string(percentage) <> "%", styles))
}

@internal
pub fn class(keyframe: Keyframe) {
  keyframe.class
}
