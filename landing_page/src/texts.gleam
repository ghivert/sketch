pub const explanation_fst = "Sketch is a universal CSS library built for Gleam. Whether you prefer CSS-in-Gleam or plain CSS, if you render your stylings on your server, or if you use node.js, bun or Erlang, Sketch covers them all!"

pub const explanation_snd = "Sketch does not try to hide CSS nor add abstraction layer on top of it. Instead, it deliberately leverages on CSS, and fills the gap between CSS and your favorite framework, be it Lustre, Wisp or even Redraw!"

pub const explanation_trd = "CSS is already the framework you need for your designs. Stop learning yet another framework you'll fight with. If you know how to write CSS, you already know how to write styles with Sketch."

pub const sketch_lustre_example = "import sketch
import sketch/lustre/html
import sketch/size.{px, percent}

pub type Color {
  Red
  Orange
  Green
}

fn button(color, text) {
  let background = case color {
    Red -> \"rgb(255, 95, 87)\"
    Orange -> \"rgb(254, 188, 46)\"
    Green -> \"rgb(40, 202, 65)\"
  }
  s.class([
    s.background(background),
    s.color(\"white\"),
    s.border_radius(px(8)),
    s.transition(\"all .3s\"),
    s.border(\"none\"),
    s.appearance(\"none\"),
    s.font_family(\"inherit\"),
    s.font_size(px(16)),
    s.padding(px(12)),
    s.font_weight(\"bold\"),
    s.cursor(\"pointer\"),
    s.min_width(px(200)),
    s.text_align(\"center\"),
    s.hover([
      s.opacity(0.7),
    ]),
  ])
  |> html.button([], [html.text(text)])
}

fn view(model: Model) {
  s.class([
    s.display(\"flex\"),
    s.gap(px(12)),
    s.height(percent(100)),
    s.flex_direction(\"column\"),
    s.align_items(\"center\"),
    s.justify_content(\"center\"),
  ])
  |> html.div([], [
    button(Red, \"Close\"),
    button(Orange, \"Minimize\"),
    button(Green, \"Fill\"),
  ])
}
"

pub const sketch_redraw_example = "import redraw
import redraw/html
import sketch
import sketch/redraw/html as sketch_html
import sketch/size.{px, percent}

pub type Color {
  Red
  Orange
  Green
}

fn button() {
  use #(color, text) <- redraw.component_(\"Button\")
  let background = case color {
    Red -> \"rgb(255, 95, 87)\"
    Orange -> \"rgb(254, 188, 46)\"
    Green -> \"rgb(40, 202, 65)\"
  }
  s.class([
    s.background(background),
    s.color(\"white\"),
    s.border_radius(px(8)),
    s.transition(\"all .3s\"),
    s.border(\"none\"),
    s.appearance(\"none\"),
    s.font_family(\"inherit\"),
    s.font_size(px(16)),
    s.padding(px(12)),
    s.font_weight(\"bold\"),
    s.cursor(\"pointer\"),
    s.min_width(px(200)),
    s.text_align(\"center\"),
    s.hover([
      s.opacity(0.7),
    ]),
  ])
  |> sketch_html.button([], [html.text(text)])
}

fn app() {
  let button = button()
  use <- redraw.component__(\"App\")
  s.class([
    s.display(\"flex\"),
    s.gap(px(12)),
    s.height(percent(100)),
    s.flex_direction(\"column\"),
    s.align_items(\"center\"),
    s.justify_content(\"center\"),
  ])
  |> sketch_html.div([], [
    button(#(Red, \"Close\")),
    button(#(Orange, \"Minimize\")),
    button(#(Green, \"Fill\")),
  ])
}
"

pub const sketch_css_example = "import sketch

fn row() {
  sketch.class([
    sketch.display(\"flex\"),
    sketch.flex_direction(\"row\"),
  ])
}

fn column() {
  sketch.class([
    sketch.display(\"flex\"),
    sketch.flex_direction(\"column\"),
  ])
}

fn gap_row(gap: String) {
  sketch.class([
    sketch.compose(row()),
    sketch.gap_(gap),
  ])
}
"

pub const sketch_css_css = ".row {
  display: flex;
  flex-direction: row;
}

.column {
  display: flex;
  flex-direction: column;
}

.gap-row {
  gap: var(--flex-direction);
}
"

pub const sketch_css_gleam = "pub const row = \"row\"

pub const column = \"column\"

pub const gap_row = \"row gap_row\"
"

pub const sketch_ssr_example = "import lustre/element
import mist
import sketch
import sketch/lustre as sketch_lustre
import sketch/lustre/html
import wisp

pub fn response() {
  html.html([], [
    html.head([], [html.title(\"Sketch Example\")]),
    html.body_([], [
      html.div(row(), [], [
        html.text(\"Hello Sketch!\"),
      ]),
    ]),
  ])
}

fn row() {
  sketch.class([
    sketch.display(\"flex\"),
    sketch.flex_direction(\"row\"),
  ])
}

pub fn main() {
  let assert Ok(cache) = sketch.cache(strategy: sketch.Ephemeral)
  let secret = wisp.random_string(64)
  let assert Ok(_) =
    fn (_request) {
      response()
      |> sketch_lustre.ssr(cache)
      |> element.to_document_string_builder
      |> wisp.html_response(200)
    }
    |> wisp_mist.handler(secret)
    |> mist.new
    |> mist.port(3000)
    |> mist.start_http
  process.sleep_forever()
}
"

pub const sketch_ssr_html = "<!doctype html>
<html>
  <head>
    <title>Sketch Example</title>
    <style>
      .css-0123456 {
        display: flex;
        flex-direction: row;
      }
    </style>
  </head>
  <body>
    <div class=\"css-0123456\">Hello Sketch!</div>
  </body>
</html>"
