import sketch/css
import sketch/css/angle.{deg}
import sketch/css/length.{px}
import sketch/css/transform

/// Multiple dimensions, with exposings or not.
pub fn dimensions_variables() {
  css.class([
    css.padding(px(12)),
    css.margin(length.px(12)),
    css.transform([
      transform.rotate(angle.rad(1.0)),
      transform.skew(deg(1.0), deg(2.0)),
    ]),
  ])
}
