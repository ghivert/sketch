import gleam/bytes_builder
import gleam/erlang/process
import gleam/http/response.{type Response}
import lustre/element
import mist.{type ResponseData}
import shared_view
import sketch
import sketch/lustre as sketch_lustre

pub fn main() {
  let assert Ok(cache) = sketch.cache(strategy: sketch.Persistent)
  let assert Ok(_) =
    fn(_) { greet(cache) }
    |> mist.new()
    |> mist.port(1234)
    |> mist.start_http()
  process.sleep_forever()
}

fn greet(cache: sketch.Cache) -> Response(ResponseData) {
  shared_view.ssr(0)
  |> sketch_lustre.ssr(cache)
  |> element.to_document_string()
  |> bytes_builder.from_string()
  |> mist.Bytes()
  |> response.set_body(response.new(200), _)
}
