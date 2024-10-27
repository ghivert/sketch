import components/button
import components/copy_button
import components/navbar
import components/windows
import layout.{body_container, main_title, row, section, title, title_container}
import redraw
import redraw/html as h
import redraw_dom/client
import sketch as s
import sketch/redraw as sr
import sketch/redraw/html as sh
import texts

pub fn main() {
  let app = app()
  let root = client.create_root("app")
  client.render(root, redraw.strict_mode([sr.provider([app()])]))
}

fn app() {
  let lustre_section = lustre_section()
  let redraw_section = redraw_section()
  let plain_css_section = plain_css_section()
  let wisp_section = wisp_section()
  let navbar = navbar.navbar()
  use <- redraw.component__("App")
  sh.div(s.class([s.font_family("Lexend")]), [], [
    navbar(),
    description_section(),
    lustre_section(),
    redraw_section(),
    plain_css_section(),
    wisp_section(),
  ])
}

fn description_section() {
  redraw.fragment([
    title_container([
      main_title("Leverage on your CSS knowledge."),
      main_title("Create your own styles."),
      main_title("Make your application Gleam."),
    ]),
    body_container([], [
      title("Sketch, CSS for anywhere"),
      h.div([], [h.text(texts.explanation_fst)]),
    ]),
    body_container([], [
      title("CSS is the framework"),
      h.div([], [h.text(texts.explanation_snd)]),
      h.div([], [h.text(texts.explanation_trd)]),
      h.div([], [h.text("Show me an example with…")]),
      row([], [
        button.primary("Lustre"),
        button.primary("Redraw"),
        button.primary("Plain CSS"),
        button.primary("SSR"),
      ]),
    ]),
  ])
}

fn lustre_section() {
  let copy_button = copy_button.copy_button()
  use <- redraw.component__("LustreSection")
  section("#eee", [
    row([], [
      copy_button.title("Sketch Lustre"),
      copy_button(#("gleam add sketch sketch_lustre")),
    ]),
    layout.windows_wrapper([
      layout.windows_row([
        windows.scaffold([
          windows.menu_bar([windows.traffic_lights()]),
          windows.editor(texts.sketch_lustre_example),
        ]),
        windows.render([
          layout.buttons_row([
            button.example(button.Red, "Close"),
            button.example(button.Orange, "Minimize"),
            button.example(button.Green, "Fill"),
          ]),
        ]),
      ]),
    ]),
  ])
}

fn redraw_section() {
  let copy_button = copy_button.copy_button()
  use <- redraw.component__("RedrawSection")
  section("#fff", [
    row([], [
      copy_button.title("Sketch Redraw"),
      copy_button(#("gleam add sketch sketch_redraw")),
    ]),
    layout.windows_wrapper([
      layout.windows_row([
        windows.scaffold([
          windows.menu_bar([windows.traffic_lights()]),
          windows.editor(texts.sketch_redraw_example),
        ]),
        windows.render([
          layout.buttons_row([
            button.example(button.Red, "Close"),
            button.example(button.Orange, "Minimize"),
            button.example(button.Green, "Fill"),
          ]),
        ]),
      ]),
    ]),
  ])
}

fn plain_css_section() {
  let copy_button = copy_button.copy_button()
  use <- redraw.component__("PlainCssSection")
  section("#eee", [
    row([], [
      copy_button.title("Sketch CSS"),
      copy_button(#("gleam add sketch sketch_css")),
    ]),
    layout.windows_wrapper([
      layout.windows_row([
        windows.scaffold([
          windows.menu_bar([windows.traffic_lights()]),
          windows.editor(texts.sketch_css_example),
        ]),
        windows.scaffold([
          windows.menu_bar([windows.traffic_lights()]),
          windows.css(texts.sketch_css_css),
        ]),
        windows.scaffold([
          windows.menu_bar([windows.traffic_lights()]),
          windows.editor(texts.sketch_css_gleam),
        ]),
      ]),
    ]),
  ])
}

fn wisp_section() {
  let copy_button = copy_button.copy_button()
  use <- redraw.component__("WispSection")
  section("#fff", [
    row([], [
      copy_button.title("Sketch SSR"),
      copy_button(#("gleam add sketch")),
    ]),
    layout.windows_wrapper([
      layout.windows_row([
        windows.scaffold([
          windows.menu_bar([windows.traffic_lights()]),
          windows.editor(texts.sketch_ssr_example),
        ]),
        windows.scaffold([
          windows.menu_bar([windows.traffic_lights()]),
          windows.html(texts.sketch_ssr_html),
        ]),
      ]),
    ]),
  ])
}
