import lustre
import shared_view
import sketch

pub fn main() {
  shared_view.app(sketch.Persistent)
  |> lustre.start("#app", Nil)
}
