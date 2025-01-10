import sketch/css
import test_/constants
import test_/helpers

/// Constants in another file.
pub fn property() {
  css.class([
    css.width_(constants.md),
    css.color(constants.blue),
    helpers.custom_color(constants.red),
  ])
}

/// Class in another file.
pub fn class() {
  css.class([
    css.compose(helpers.card_body(constants.red)),
    css.background("green"),
  ])
}
