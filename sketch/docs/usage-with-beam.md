# Usage with BEAM

Sketch can be transparently used with BEAM and JS, however, due to the runtime,
some differences exists, and should be taken in consideration. Contrarily to JS,
BEAM is a runtime which is actor oriented, and synchronous.

To generate the real stylesheet the application will use, sketch uses an
intermediate virtual stylesheet, called "cache". In BEAM, a cache is a process,
storing its own state.

Just like with any app, creating a cache relies on
[`cache`](https://hexdocs.pm/sketch/sketch.html#cache). Once created, a cache
will live as long as you not destroy it. To access the generated stylesheet,
[`render`](https://hexdocs.pm/sketch/sketch.html#render) will turn the cache
into a proper stylesheet, i.e. it will returns a `String`, containing the
stylesheet content. All you have to do then is to embed the resulting string in
your output HTML, and voilà!

## Example

The example assumes you're using a blank, empty
[`mist`](https://hexdocs.pm/mist) server. It could feel a little cumbersome, but
most of the time, your framework should handle it directly.

```gleam
import gleam/bytes_builder
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/string
import mist.{type Connection, type ResponseData}
import sketch
import sketch/options as sketch_options

pub fn main() {
  // Creates the cache here. A global cache will be used here, because the SSR
  // only uses one process.
  let assert Ok(cache) = sketch.cache(strategy: sketch.Persistent)

  // Helpers for mist server.
  let empty_body = mist.Bytes(bytes_builder.new())
  let not_found = response.set_body(response.new(404), empty_body)

  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      // Before every request, prepare the cache, to wipe it from old data, and
      // mark it as "default" cache for the following render.
      case request.path_segments(req) {
        // Send the cache to your render function. It will be used before
        // returning the HTML.
        ["greet", name] -> greet(name, cache)
        _ -> not_found
      }
    }
    |> mist.new()
    |> mist.port(3000)
    |> mist.start_http()

  process.sleep_forever()
}

// Defines a class, here with simple class name.
fn main_class(cache) {
  [sketch.color("red")]
  |> sketch.class()
  |> sketch.to_class_name(cache)
}

fn greet(name: String, cache: sketch.Cache) -> Response(ResponseData) {
  let res = response.new(200)
  let greetings = "<div>Hey there, " <> name <> "!</div>"

  // Create your body, with the classes computed.
  let #(class_name, cache) = main_class(cache)
  let body =
    [
      "<div class=" <> class_name <> ">",
      "  <div>Hello World!</div>",
      greetings,
      "</div>",
    ]
    |> string.join("\n")

  // Get the content of your stylesheet.
  let assert Ok(stylesheet_content) = sketch.render(cache)

  // Render the style in your HTML.
  let html =
    [
      "<html>",
      "  <head>",
      "    <style>",
      stylesheet_content,
      "    </style>",
      "  </head>",
      "  <body>",
      body,
      "  </body>",
      "</html>",
    ]
    |> string.join("\n")

  html
  |> bytes_builder.from_string()
  |> mist.Bytes()
  |> response.set_body(res, _)
}
```

## More details

You can find more details on how it works in the
[internal details](https://hexdocs.pm/sketch/internal-details.html).
