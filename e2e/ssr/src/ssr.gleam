import gleam/bytes_builder
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import lustre/element
import mist.{type Connection, type ResponseData}
import sketch
import sketch/lustre as sketch_lustre
import sketch/lustre/element/html

pub fn main() {
  let assert Ok(cache) = sketch.cache(strategy: sketch.Persistent)
  let assert Ok(_) =
    handler(cache, _)
    |> mist.new()
    |> mist.port(3000)
    |> mist.start_http()
  process.sleep_forever()
}

fn handler(cache, req: Request(Connection)) -> Response(ResponseData) {
  case request.path_segments(req) {
    ["greet", name] -> greet(name, cache)
    _ -> {
      bytes_builder.new()
      |> mist.Bytes
      |> response.set_body(response.new(404), _)
    }
  }
}

fn page(greetings) {
  let main_class = fn(a, c) { html.h1(a, c, [sketch.color("red")]) }
  html.html([], [
    html.head([], [html.title([], "Greetings!")]),
    html.body_([], [main_class([], [html.text(greetings)])]),
  ])
}

fn greet(name: String, cache: sketch.Cache) -> Response(ResponseData) {
  page("Hey there, " <> name <> "!")
  |> sketch_lustre.ssr(cache)
  |> element.to_document_string()
  |> bytes_builder.from_string()
  |> mist.Bytes()
  |> response.set_body(response.new(200), _)
}
