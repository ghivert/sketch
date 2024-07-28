import lustre
import shared_view

pub fn main() {
  shared_view.app()
  |> lustre.start("#app", Nil)
}
