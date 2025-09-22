import ffi
import gleam/bool
import icons
import redraw
import redraw/dom/events
import redraw/dom/html as h
import sketch/css
import sketch/css/length.{px, rem}
import sketch/redraw/dom/hooks/html as shh
import sketch/redraw/dom/html as sh

pub fn copy_button() {
  use #(text) <- redraw.element("CopyButton")
  let #(copied, on_copy) = use_copy(text)
  sh.code(code_install(), [on_copy], [
    h.text(text),
    sh.button(sm_button_class(), [on_copy], [
      icons.tiny(case copied {
        True -> icons.check()
        False -> icons.copy()
      }),
    ]),
  ])
}

fn use_copy(text: String) {
  let #(copied, set_copied) = redraw.use_state(False)
  use_copied_timeout(copied, set_copied)
  #(copied, on_copy(text, set_copied))
}

fn use_copied_timeout(copied: Bool, set_copied: fn(Bool) -> Nil) -> Nil {
  use <- redraw.use_effect_(_, #(copied))
  use <- bool.guard(when: !copied, return: fn() { Nil })
  let timeout = ffi.set_timeout(fn() { set_copied(False) }, 2000)
  fn() { ffi.clear_timeout(timeout) }
}

fn on_copy(text, set_copied) {
  use _ <- events.on_click
  ffi.clipboard_copy(text)
  set_copied(True)
}

fn code_install() {
  css.class([
    css.border("1px solid var(--border-color)"),
    css.border_radius(px(8)),
    css.display("flex"),
    css.align_items("center"),
    css.padding(px(2)),
    css.padding_left(px(8)),
    css.gap(px(9)),
    css.font_size(rem(0.7)),
    css.font_weight("450"),
    css.background("var(--background)"),
    css.cursor("pointer"),
    css.color("var(--text-color)"),
    css.hover([css.background("var(--button-hover)")]),
  ])
}

fn sm_button_class() {
  css.class([
    css.background("var(--dark-background)"),
    css.border_radius(px(6)),
    css.padding(px(4)),
    css.color("inherit"),
  ])
}

pub fn title(text) {
  use <- shh.h3([], [h.text(text)])
  css.class([css.font_size(rem(1.8)), css.font_weight("600")])
}
