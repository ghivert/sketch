import gleam/bytes_tree
import gleam/erlang/process
import gleam/http/response.{type Response}
import lustre/element
import mist.{type ResponseData}
import shared_view

pub fn main() {
  let assert Ok(_) =
    fn(_) { greet() }
    |> mist.new()
    |> mist.port(1234)
    |> mist.start_http()
  process.sleep_forever()
}

fn greet() -> Response(ResponseData) {
  shared_view.ssr(0)
  |> element.to_document_string()
  |> bytes_tree.from_string()
  |> mist.Bytes()
  |> response.set_body(response.new(200), _)
}
