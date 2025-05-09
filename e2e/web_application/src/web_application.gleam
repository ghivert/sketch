import lustre
import shared_view
import sketch/lustre as sketch_lustre

pub fn main() {
  let assert Ok(stylesheet) = sketch_lustre.setup()
  shared_view.app(stylesheet)
  |> lustre.start("#app", Nil)
}
