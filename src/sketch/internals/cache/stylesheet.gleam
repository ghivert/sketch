import gleam/dict.{type Dict}
import gleam/string

pub type StyleSheet {
  StyleSheet(counter: Int, rules: Dict(Int, String))
}

pub fn init() {
  StyleSheet(counter: 0, rules: dict.new())
}

pub fn insert_styles(
  stylesheet: StyleSheet,
  class: String,
) -> #(StyleSheet, Int) {
  let counter = stylesheet.counter
  stylesheet.rules
  |> dict.insert(counter, class)
  |> fn(r) { #(StyleSheet(counter: counter + 1, rules: r), counter) }
}

pub fn delete_styles(stylesheet: StyleSheet, counter: Int) -> StyleSheet {
  stylesheet.rules
  |> dict.delete(counter)
  |> fn(r) { StyleSheet(counter: counter + 1, rules: r) }
}

pub fn render(stylesheet: StyleSheet) -> String {
  stylesheet.rules
  |> dict.values()
  |> string.join("\n\n")
}
