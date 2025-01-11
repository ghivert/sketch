import sketch/css
import sketch/css/font_face

pub fn font_face() {
  css.font_face([
    font_face.src("file"),
    font_face.font_family("Example"),
    font_face.font_style("bold"),
  ])
}
