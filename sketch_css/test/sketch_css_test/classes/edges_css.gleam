import sketch/css

pub fn edge_cases(custom) {
  css.class([
    css.grid_template_areas(["header", "main"]),
    css.property("--example-property", "example-value"),
    css.color(custom),
  ])
}
