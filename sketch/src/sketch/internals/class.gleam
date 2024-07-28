//// BEAM only.

import gleam/list
import gleam/option.{type Option, None, Some}

pub type Definitions {
  Definitions(
    medias_def: List(String),
    selectors_def: List(String),
    class_def: String,
  )
}

pub opaque type Content {
  Content(
    class_name: String,
    class_id: Int,
    definitions: Definitions,
    rules: Option(List(Int)),
  )
}

pub fn no_class() {
  let defs = Definitions(medias_def: [], selectors_def: [], class_def: "")
  Content(class_name: "", class_id: -1, definitions: defs, rules: None)
}

pub fn class_id(class: Content) {
  class.class_id
}

pub fn class_name(class: Content) {
  class.class_name
}

pub fn rules(class: Content) {
  class.rules
}

pub fn definitions(class: Content) {
  let Definitions(medias, selectors, class) = class.definitions
  [[class], selectors, medias]
  |> list.concat()
}

pub fn set_rules(class: Content, rules: List(Int)) {
  Content(..class, rules: Some(rules))
}

pub fn create(
  class_name class_name: String,
  class_id class_id: Int,
  rules rules: Option(List(Int)),
  definitions definitions: Definitions,
) {
  Content(
    class_name: class_name,
    class_id: class_id,
    definitions: definitions,
    rules: rules,
  )
}
