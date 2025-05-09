import sketch/css
import sketch/css/length.{px}

pub fn root(color: String) -> css.Global {
  css.global(":root", [
    css.color(color),
    css.background("blue"),
    css.margin(px(0)),
  ])
}
