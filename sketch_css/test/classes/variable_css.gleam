import sketch/css

pub fn variable_property() {
  let red = "red"
  css.class([
    // Variable property, should be replaced.
    css.background(red),
  ])
}
