import icons/book_open
import icons/check
import icons/copy
import icons/github
import icons/home
import sketch/css
import sketch/css/length.{px}
import sketch/redraw/dom/hooks/html as h

pub fn small(icon) {
  use <- h.div([], [icon])
  css.class([css.width(px(24)), css.height(px(24))])
}

pub fn tiny(icon) {
  use <- h.div([], [icon])
  css.class([css.width(px(12)), css.height(px(12))])
}

pub const book_open = book_open.icon

pub const check = check.icon

pub const copy = copy.icon

pub const github = github.icon

pub const home = home.icon
