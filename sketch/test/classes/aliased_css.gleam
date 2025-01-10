import sketch/css as s

pub fn aliased_module() {
  // Aliased property, should be rewrote.
  s.class([s.background("red")])
}
