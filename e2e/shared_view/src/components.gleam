//// Defines the base components used in the shared view. Think copmonents as in
//// functions that simply returns the HTML correctly formatted.
//// Every component accepts two arrays, attributes and children, to follow the
//// same convention as Lustre standard HTML. That way, you could leverage
//// your knowledge of Lustre, and behaves exactly as expected.

import shared_styles as styles
import sketch/lustre/element
import sketch/lustre/element/html

pub fn body(attrs, children) {
  // demonstrate ability to merge fragment at root
  element.fragment([
    html.div(styles.body_style(), attrs, children),
    html.footer_([], []),
  ])
}

pub fn topbar(attrs, children) {
  html.div(styles.topbar_style(), attrs, children)
}

pub fn headline(value, attrs, children) {
  html.main(styles.headline_style(value), attrs, children)
}

pub fn headline_subtitle(attrs, children) {
  html.div(styles.headline_subtitle_style(), attrs, children)
}

pub fn headline_emphasize(attrs, children) {
  html.div(styles.headline_emphasize_style(), attrs, children)
}

pub fn counter(attrs, children) {
  html.div(styles.counter_style(), attrs, children)
}

pub fn counter_title(attrs, children) {
  html.div_(attrs, children)
}

pub fn counter_subtitle(attrs, children) {
  html.div(styles.counter_subtitle_style(), attrs, children)
}

pub fn button(attrs, children) {
  html.button(styles.button_style(), attrs, children)
}

pub fn value(attrs, children) {
  html.div(styles.value_style(), attrs, children)
}

pub fn value_content(attrs, children) {
  html.div(styles.value_content_style(), attrs, children)
}

pub fn showcase(attrs, children) {
  html.div(styles.showcase_style(), attrs, children)
}

pub fn counter_body(attrs, children) {
  html.div(styles.counter_body_style(), attrs, children)
}

pub fn counter_body_title(attrs, children) {
  html.div(styles.counter_body_title_style(), attrs, children)
}

pub fn counter_counter(attrs, children) {
  html.div(styles.counter_counter_style(), attrs, children)
}

pub fn showcase_body(attrs, children) {
  html.div(styles.showcase_body_style(), attrs, children)
}

pub fn card_title(attrs, children) {
  html.div_(attrs, children)
}
