import gleam/string
import gleam/option.{type Option}
import sketch/internals/string as sketch_string

pub fn wrap_class(
  id: String,
  properties: List(String),
  idt: Int,
  pseudo: Option(String),
) {
  let base_indent = sketch_string.indent(idt)
  let pseudo_ = option.unwrap(pseudo, "")
  [base_indent <> "." <> id <> pseudo_ <> " {", ..properties]
  |> string.join("\n")
  |> string.append(base_indent <> "}")
}
