import icons/book_open
import icons/check
import icons/copy
import icons/github
import icons/home
import sketch/css
import sketch/css/length.{px}
import sketch/redraw/dom/html as h

pub fn small(icon) {
  css.class([css.width(px(24)), css.height(px(24))])
  |> h.div([], [icon])
}

pub fn tiny(icon) {
  css.class([css.width(px(12)), css.height(px(12))])
  |> h.div([], [icon])
}

pub const book_open = book_open.icon

pub const check = check.icon

pub const copy = copy.icon

pub const github = github.icon

pub const home = home.icon
