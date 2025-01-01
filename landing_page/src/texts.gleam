pub const explanation_fst = "Sketch is a universal CSS library built for Gleam. Whether you prefer CSS-in-Gleam or plain CSS, if you render your stylings on your server, or if you use node.js, bun or Erlang, Sketch covers them all!"

pub const explanation_snd = "Sketch does not try to hide CSS nor add abstraction layer on top of it. Instead, it deliberately leverages on CSS, and fills the gap between CSS and your favorite framework, be it Lustre, Wisp or even Redraw! "

pub const explanation_trd = "CSS is already the framework you need for your designs. Stop learning yet another framework you'll fight with. If you know how to write CSS, you already know how to write styles with Sketch."

pub const bring_me_docs = "Documentation"

pub const sketch_lustre_fst = "Lustre being the most popular frontend framework in Gleam, Sketch has an implementation working with it!"

pub const sketch_lustre_snd = "Simply add the package, and start using Lustre elements, Sketch enhanced! Put some styles directly on elements, and watch them automatically applied in your resulting HTML!"

pub const sketch_redraw_fst = "Redraw is a React bindings to Gleam. And as such, Redraw deserves a correct CSS-in-Gleam package. That's where Sketch comes into play!"

pub const sketch_redraw_snd = "If you're used to styled components or emotion, then you already know how to use sketch_redraw! If you're used to Lustre, you shouldn't be suprised neither! In any case, install the package, and start styling your components!"

pub const sketch_css_fst = "Sketch is a thinner layer over CSS. Because of this, targeting CSS from Sketch seemed natural!"

pub const sketch_css_snd = "Contrarily to most CSS-in-JS package, Sketch also allows you to generate CSS. And as such, it's easy to share your styles written in Sketch in any project! Everything is all Sketch classes all the way, and they can be used in any situation!"

pub const sketch_wisp_fst = "CSS-in-Gleam is great, but sometimes, you just need to output some CSS from your server. Sketch got you cover too."

pub const sketch_wisp_snd = "Sketch can be used with Lustre, but also with any way to generate HMTL and CSS. All you need is creating a StyleSheet, and putting some styles inside. Once it's done, render the stylesheet as string, and you have a valid CSS file! And of course, because Lustre can also handle SSR for you, Sketch can integrate in Lustre's SSR!"

pub const sketch_lustre_example = "import sketch/css
import sketch/css/size.{px, percent}
import sketch/lustre/html

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
  css.class([
    css.background(background),
    css.color(\"white\"),
    css.border_radius(px(8)),
    css.transition(\"all .3s\"),
    css.border(\"none\"),
    css.appearance(\"none\"),
    css.font_family(\"inherit\"),
    css.font_size(px(16)),
    css.padding(px(12)),
    css.font_weight(\"bold\"),
    css.cursor(\"pointer\"),
    css.min_width(px(200)),
    css.text_align(\"center\"),
    css.hover([
      css.opacity(0.7),
    ]),
  ])
  |> html.button([], [html.text(text)])
}

fn view(model: Model) {
  css.class([
    css.display(\"flex\"),
    css.gap(px(12)),
    css.height(percent(100)),
    css.flex_direction(\"column\"),
    css.align_items(\"center\"),
    css.justify_content(\"center\"),
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
import sketch/css
import sketch/css/size.{px, percent}
import sketch/redraw/html as sketch_html

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
  css.class([
    css.background(background),
    css.color(\"white\"),
    css.border_radius(px(8)),
    css.transition(\"all .3s\"),
    css.border(\"none\"),
    css.appearance(\"none\"),
    css.font_family(\"inherit\"),
    css.font_size(px(16)),
    css.padding(px(12)),
    css.font_weight(\"bold\"),
    css.cursor(\"pointer\"),
    css.min_width(px(200)),
    css.text_align(\"center\"),
    css.hover([
      css.opacity(0.7),
    ]),
  ])
  |> sketch_html.button([], [html.text(text)])
}

fn app() {
  let button = button()
  use <- redraw.component__(\"App\")
  css.class([
    css.display(\"flex\"),
    css.gap(px(12)),
    css.height(percent(100)),
    css.flex_direction(\"column\"),
    css.align_items(\"center\"),
    css.justify_content(\"center\"),
  ])
  |> sketch_html.div([], [
    button(#(Red, \"Close\")),
    button(#(Orange, \"Minimize\")),
    button(#(Green, \"Fill\")),
  ])
}
"

pub const sketch_css_example = "import sketch/css

fn row() {
  css.class([
    css.display(\"flex\"),
    css.flex_direction(\"row\"),
  ])
}

fn column() {
  css.class([
    css.display(\"flex\"),
    css.flex_direction(\"column\"),
  ])
}

fn gap_row(gap: String) {
  css.class([
    css.compose(row()),
    css.gap_(gap),
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
import sketch/css
import sketch/lustre as sketch_lustre
import sketch/lustre/html
import wisp

pub fn response(stylesheet: sketch.StyleSheet) {
  use <- sketch_lustre.ssr(stylesheet)
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
  css.class([
    css.display(\"flex\"),
    css.flex_direction(\"row\"),
  ])
}

pub fn main() {
  let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Ephemeral)
  let secret = wisp.random_string(64)
  let assert Ok(_) =
    fn (_request) {
      response(stylesheet)
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
