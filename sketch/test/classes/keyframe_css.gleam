import sketch/css
import sketch/css/keyframe

pub fn keyframe() {
  css.keyframes("fade-out", [
    keyframe.from([css.opacity(1.0)]),
    keyframe.at(50, [css.opacity(0.5)]),
    keyframe.to([css.opacity(0.0)]),
  ])
}

pub fn example() {
  css.class([
    css.opacity(1.0),
    css.animation("fade-out"),
    css.background("blue"),
  ])
}
