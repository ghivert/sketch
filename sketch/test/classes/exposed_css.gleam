import sketch/css.{background, class as class_}

pub fn exposed_property() {
  css.class([
    // Exposed property, should be rewritten correctly.
    background("red"),
    css.color("red"),
  ])
}

pub fn exposed_class() {
  // Exposed class, should be rewritten correctly.
  class_([background("red"), css.color("red")])
}
