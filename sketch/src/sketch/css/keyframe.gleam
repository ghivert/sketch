//// The `@keyframes` CSS [at-rule](https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule)
//// controls the intermediate steps in a CSS  animation sequence by defining
//// styles for keyframes (or waypoints) along the animation sequence. This
//// gives more control over the intermediate steps of the animation sequence
//// than [transitions](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_transitions).
////
//// ---
////
//// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@keyframes)

import gleam/int
import sketch/internals/cache/cache as style

/// A keyframe is a part of an `@keyframes` rule.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@keyframes)
pub opaque type Keyframe {
  Keyframe(class: style.Class)
}

/// A starting offset of `0%`.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@keyframes#from)
pub fn from(styles: List(style.Style)) {
  Keyframe(style.named("from", styles))
}

/// An ending offset of `100%`.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@keyframes#to)
pub fn to(styles: List(style.Style)) {
  Keyframe(style.named("to", styles))
}

/// A percentage of the time through the animation sequence at which the
/// specified keyframe should occur.
///
/// ---
///
/// [MDN Reference](https://developer.mozilla.org/docs/Web/CSS/@keyframes#percentage)
pub fn at(percentage: Int, styles: List(style.Style)) {
  Keyframe(style.named(int.to_string(percentage) <> "%", styles))
}

/// Internal function, can be used if you need to go from a keyframe to a String
/// in case you're building on top of sketch.
@internal
pub fn class(keyframe: Keyframe) -> style.Class {
  keyframe.class
}
