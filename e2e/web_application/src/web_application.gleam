import lustre
import shared_view
import sketch
import sketch/lustre/experimental as sketch_lustre

pub fn main() {
  let assert Ok(stylesheet) = sketch.stylesheet(sketch.Persistent)
  let assert Ok(_) = sketch_lustre.setup(stylesheet)
  shared_view.app()
  |> lustre.start("#app", Nil)
}
