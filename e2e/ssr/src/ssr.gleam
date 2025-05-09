import gleam/bytes_tree
import gleam/erlang/process
import gleam/http/response.{type Response}
import lustre/element
import mist.{type ResponseData}
import shared_view
import sketch
import sketch/lustre/experimental as sketch_lustre

pub fn main() {
  let assert Ok(stylesheet) = sketch.stylesheet(sketch.Persistent)
  let assert Ok(_) = sketch_lustre.setup(stylesheet)
  let assert Ok(_) =
    fn(_) { greet(stylesheet) }
    |> mist.new()
    |> mist.port(1234)
    |> mist.start_http()
  process.sleep_forever()
}

fn greet(stylesheet: sketch.StyleSheet) -> Response(ResponseData) {
  shared_view.ssr(0, stylesheet)
  |> element.to_document_string()
  |> bytes_tree.from_string()
  |> mist.Bytes()
  |> response.set_body(response.new(200), _)
}
