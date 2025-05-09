// IMPORTS ---------------------------------------------------------------------

import gleam/bytes_tree
import gleam/erlang
import gleam/erlang/process.{type Selector, type Subject}
import gleam/function
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html.{html}
import lustre/server_component
import mist.{type Connection, type ResponseData}
import shared_view
import sketch
import sketch/css
import sketch/lustre/experimental as sketch_lustre

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(stylesheet) = sketch.stylesheet(sketch.Persistent)
  let assert Ok(_) =
    stylesheet
    |> sketch.global(host_class())
    |> sketch_lustre.setup
  let assert Ok(_) =
    fn(request: Request(Connection)) -> Response(ResponseData) {
      // In order to get started with server components, we'll need to handle at
      // least three things:
      case request.path_segments(request) {
        // 1. Serving the HTML document that will render the `<lustre-server-component />`
        //    custom element.
        [] -> serve_html()
        // 2. Serving the pre-build JavaScript runtime that registers the custom
        //    element and handles communication and rendering.
        ["lustre", "runtime.mjs"] -> serve_runtime()
        // 3. The websocket connection that the client runtime will connect to
        //    and the server runtime can push messages to.
        ["ws"] -> serve_counter(request)
        _ -> response.set_body(response.new(404), mist.Bytes(bytes_tree.new()))
      }
    }
    |> mist.new
    |> mist.bind("localhost")
    |> mist.port(1234)
    |> mist.start_http

  process.sleep_forever()
}

// HTML ------------------------------------------------------------------------

fn serve_html() -> Response(ResponseData) {
  let html =
    html([attribute.lang("en")], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),
        html.title([], "06-server-components/01-basic-setup"),
        html.script(
          // When serving the client runtime for server components, you must
          // remember to set the `type` attribute to `"module"` otherwise it won't
          // work!
          [attribute.type_("module"), attribute.src("/lustre/runtime.mjs")],
          "",
        ),
        html.link([
          attribute.rel("preconnect"),
          attribute.href("https://fonts.googleapis.com"),
        ]),
        html.link([
          attribute.rel("preconnect"),
          attribute.href("https://fonts.gstatic.com"),
          attribute.crossorigin("true"),
        ]),
        html.link([
          attribute.rel("stylesheet"),
          attribute.href(
            "https://fonts.googleapis.com/css2?family=Lexend:wght@100..900&family=Zain:wght@200;300;400;700;800;900&display=swap",
          ),
        ]),
      ]),
      html.body(
        [attribute.styles([#("margin", "0"), #("font-family", "Zain")])],
        // If you're using Lustre to render your HTML like we are here, or if
        // you're also running Lustre in the browser, youcan render the
        // `<lustre-server-component />` element using `server_component.element`.
        //
        // The server_component module also includes attributes that are relevant
        // to the component. The `server_component.route` attribute tells the
        // client runtime where to make the websocket connection to the server.
        // This path should match the path we used in our mist handler above.
        [server_component.element([server_component.route("/ws")], [])],
      ),
    ])
    |> element.to_document_string_tree
    |> bytes_tree.from_string_tree

  response.new(200)
  |> response.set_body(mist.Bytes(html))
  |> response.set_header("content-type", "text/html")
}

// JAVASCRIPT ------------------------------------------------------------------

fn serve_runtime() -> Response(ResponseData) {
  // Whenever you want to use server components, it's important that you serve
  // the client runtime to the browser. This small JavaScript module registers
  // the `<lustre-server-component />` custom element that you'll use in your HTML
  // to create server components.
  //
  // Lustre includes both a standard and a minified version of the runtime. The
  // minified bundle clocks in at just 10kB before compression!
  let assert Ok(lustre_priv) = erlang.priv_directory("lustre")
  let file_path = lustre_priv <> "/static/lustre-server-component.min.mjs"

  case mist.send_file(file_path, offset: 0, limit: None) {
    Ok(file) ->
      response.new(200)
      |> response.prepend_header("content-type", "application/javascript")
      |> response.set_body(file)

    Error(_) ->
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_tree.new()))
  }
}

// WEBSOCKET -------------------------------------------------------------------

fn serve_counter(request: Request(Connection)) -> Response(ResponseData) {
  mist.websocket(
    request:,
    on_init: init_counter_socket,
    handler: loop_counter_socket,
    on_close: close_counter_socket,
  )
}

type CounterSocket {
  CounterSocket(
    component: lustre.Runtime(shared_view.Msg),
    self: Subject(server_component.ClientMessage(shared_view.Msg)),
  )
}

type CounterSocketMessage =
  server_component.ClientMessage(shared_view.Msg)

type CounterSocketInit =
  #(CounterSocket, Option(Selector(CounterSocketMessage)))

fn init_counter_socket(_) -> CounterSocketInit {
  let counter = shared_view.app()
  // Rather than calling `lustre.start` as we do in the client, we construct the
  // Lustre runtime by calling `lustre.start_server_component`. This is the same
  // `Runtime` type we get from `lustre.start` but this function doesn't need a
  // CSS selector for the element to attach to: there's no DOM here!
  let assert Ok(component) = lustre.start_server_component(counter, Nil)

  // The server component runtime communicates to the websocket process using
  // Gleam's standard process messaging. We construct a new subject that the
  // runtime can send messages to, and then we initialise a selector so that we
  // can handle those messages in `loop_counter_socket`.
  let self = process.new_subject()
  let selector =
    process.new_selector()
    |> process.selecting(self, function.identity)

  // Calling `register_subject` is how the runtime knows to send messages to
  // this process when it wants to communicate with the client. In Lustre, server
  // components are not opinionated about the transport layer or your network
  // setup: instead the runtime broadcasts messages to any registered subjects
  // and lets you handle the transport layer yourself.
  server_component.register_subject(self)
  |> lustre.send(to: component)

  #(CounterSocket(component:, self:), Some(selector))
}

fn loop_counter_socket(
  state: CounterSocket,
  connection: mist.WebsocketConnection,
  message: mist.WebsocketMessage(CounterSocketMessage),
) {
  case message {
    // The client runtime will send us JSON-encoded text frames that we need to
    // decode and pass to the server component runtime.
    mist.Text(json) -> {
      case json.parse(json, server_component.runtime_message_decoder()) {
        Ok(runtime_message) -> lustre.send(state.component, runtime_message)
        // This case will only be hit if something other than Lustre's client
        // runtime sends us a message.
        Error(_) -> Nil
      }

      actor.continue(state)
    }

    mist.Binary(_) -> {
      actor.continue(state)
    }

    // We hit this case when the server component runtime sends us a message that
    // we need to forward to the client. Because Lustre does not control your
    // network connection, it's our app's responsibility to make sure these messages
    // are encoded and sent to the client.
    mist.Custom(client_message) -> {
      let json = server_component.client_message_to_json(client_message)
      let assert Ok(_) = mist.send_text_frame(connection, json.to_string(json))

      actor.continue(state)
    }

    mist.Closed | mist.Shutdown -> {
      // The server component runtime sets up a process monitor that can clean
      // up if our socket process dies or is killed, but it's good practice to
      // clean up ourselves if we get the opportunity.
      server_component.deregister_subject(state.self)
      |> lustre.send(to: state.component)

      actor.Stop(process.Normal)
    }
  }
}

fn close_counter_socket(state: CounterSocket) -> Nil {
  server_component.deregister_subject(state.self)
  |> lustre.send(to: state.component)
}

fn host_class() {
  css.global(":host", [
    css.property("--atomic-tangerine", "#f79256"),
    css.property("--aquamarine", "#acfcd9"),
    css.property("--turquoise", "#5dd9c1"),
    css.property("--periwinkle", "#a6b1e1"),
    css.property("--purple", "#dcd6f7"),
  ])
}
