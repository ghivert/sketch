import components/button
import components/copy_button
import components/footer
import components/navbar
import components/windows
import ffi
import layout.{
  body_container, column, main_title, row, section, section_explanation, title,
  title_container, title_container_inside, width_container,
}
import redraw
import redraw/dom/attribute as a
import redraw/dom/client
import redraw/dom/events
import redraw/dom/html as h
import sketch/css
import sketch/css/length.{px}
import sketch/redraw as sr
import sketch/redraw/dom/html as sh
import texts

pub fn main() {
  let app = app()
  let assert Ok(root) = client.create_root("app")
  client.render(root, redraw.strict_mode([sr.provider([app()])]))
}

fn app() {
  let lustre_section = lustre_section()
  let redraw_section = redraw_section()
  let plain_css_section = plain_css_section()
  let wisp_section = wisp_section()
  let navbar = navbar.navbar()
  let footer = footer.footer()
  use <- redraw.component__("App")
  sh.div(css.class([css.font_family("Lexend")]), [], [
    navbar(),
    description_section(),
    lustre_section(),
    redraw_section(),
    plain_css_section(),
    wisp_section(),
    footer(),
  ])
}

fn description_section() {
  redraw.fragment([
    title_container([
      h.img([
        a.src("/logo.png"),
        a.style([
          #("width", "150px"),
          #("height", "150px"),
          #("transform", "rotate(-10deg)"),
        ]),
      ]),
      title_container_inside([
        main_title("Leverage your CSS knowledge."),
        main_title("Create your own styles."),
        main_title("Make your application Gleaming."),
      ]),
    ]),
    width_container([
      body_container([], [
        title("Sketch, CSS for anywhere"),
        h.div([], [h.text(texts.explanation_fst)]),
      ]),
    ]),
    width_container([
      body_container([], [
        title("CSS is the framework"),
        h.div([], [h.text(texts.explanation_snd)]),
        h.div([], [h.text(texts.explanation_trd)]),
        h.div([], [h.text("Show me an example with…")]),
        row([], [
          button.primary([scroll_to("lustre-section")], "Lustre"),
          button.primary([scroll_to("redraw-section")], "Redraw"),
          button.primary([scroll_to("plain-css-section")], "Plain CSS"),
          button.primary([scroll_to("wisp-section")], "SSR"),
        ]),
      ]),
    ]),
  ])
}

fn scroll_to(id: String) {
  use _event <- events.on_click()
  ffi.scroll_to(id)
}

fn lustre_section() {
  let copy_button = copy_button.copy_button()
  use <- redraw.component__("LustreSection")
  section("lustre-section", "var(--dark-background)", [
    width_container([
      column([], [
        copy_button.title("Sketch Lustre"),
        copy_button(#("gleam add sketch sketch_lustre")),
      ]),
      layout.windows_wrapper(px(1000), [
        column([], [
          section_explanation([], [h.text(texts.sketch_lustre_fst)]),
          section_explanation([], [h.text(texts.sketch_lustre_snd)]),
          button.link("https://hexdocs.pm/sketch_lustre", texts.bring_me_docs),
        ]),
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
    ]),
  ])
}

fn redraw_section() {
  let copy_button = copy_button.copy_button()
  use <- redraw.component__("RedrawSection")
  section("redraw-section", "var(--background)", [
    width_container([
      column([], [
        copy_button.title("Sketch Redraw"),
        copy_button(#("gleam add sketch sketch_redraw")),
      ]),
      layout.windows_wrapper(px(1000), [
        column([], [
          section_explanation([], [h.text(texts.sketch_redraw_fst)]),
          section_explanation([], [h.text(texts.sketch_redraw_snd)]),
          button.link("https://hexdocs.pm/sketch_redraw", texts.bring_me_docs),
        ]),
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
    ]),
  ])
}

fn plain_css_section() {
  let copy_button = copy_button.copy_button()
  use <- redraw.component__("PlainCssSection")
  section("plain-css-section", "var(--dark-background)", [
    width_container([
      column([], [
        copy_button.title("Sketch CSS"),
        copy_button(#("gleam add sketch sketch_css")),
      ]),
      layout.windows_wrapper(px(1400), [
        column([], [
          section_explanation([], [h.text(texts.sketch_css_fst)]),
          section_explanation([], [h.text(texts.sketch_css_snd)]),
          button.link("https://hexdocs.pm/sketch_css", texts.bring_me_docs),
        ]),
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
    ]),
  ])
}

fn wisp_section() {
  let copy_button = copy_button.copy_button()
  use <- redraw.component__("WispSection")
  section("wisp-section", "var(--background)", [
    width_container([
      column([], [
        copy_button.title("Sketch SSR"),
        copy_button(#("gleam add sketch")),
      ]),
      layout.windows_wrapper(px(1400), [
        column([], [
          section_explanation([], [h.text(texts.sketch_wisp_fst)]),
          section_explanation([], [h.text(texts.sketch_wisp_snd)]),
          button.link("https://hexdocs.pm/sketch_lustre", texts.bring_me_docs),
        ]),
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
    ]),
  ])
}
