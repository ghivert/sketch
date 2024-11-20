import ffi
import gleam/bool
import icons
import redraw
import redraw/handler
import redraw/html as h
import sketch as s
import sketch/redraw/html as sh
import sketch/size.{px, rem}

pub fn copy_button() {
  use #(text) <- redraw.component_("CopyButton")
  let #(copied, set_copied) = redraw.use_state(False)
  let on_copy = on_copy(text, set_copied)
  redraw.use_effect_(
    fn() {
      use <- bool.guard(when: !copied, return: fn() { Nil })
      let timeout = ffi.set_timeout(fn() { set_copied(False) }, 2000)
      fn() { ffi.clear_timeout(timeout) }
    },
    #(copied),
  )
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

fn on_copy(text, set_copied) {
  use _ <- handler.on_click
  ffi.clipboard_copy(text)
  set_copied(True)
}

fn code_install() {
  s.class([
    s.border("1px solid var(--border-color)"),
    s.border_radius(px(8)),
    s.display("flex"),
    s.align_items("center"),
    s.padding(px(2)),
    s.padding_left(px(8)),
    s.gap(px(9)),
    s.font_size(rem(0.7)),
    s.font_weight("450"),
    s.background("var(--background)"),
    s.cursor("pointer"),
    s.color("var(--text-color)"),
    s.hover([s.background("var(--button-hover)")]),
  ])
}

fn sm_button_class() {
  s.class([
    s.background("var(--dark-background)"),
    s.border_radius(px(6)),
    s.padding(px(4)),
    s.color("inherit"),
  ])
}

pub fn title(text) {
  s.class([s.font_size(rem(1.8)), s.font_weight("600")])
  |> sh.h3([], [h.text(text)])
}
