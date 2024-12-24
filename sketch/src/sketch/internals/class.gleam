//// BEAM only.

import gleam/list
import gleam/option.{type Option, Some}

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

pub fn new(
  class_name class_name: String,
  class_id class_id: Int,
  rules rules: Option(List(Int)),
  definitions definitions: Definitions,
) -> Content {
  Content(
    class_name: class_name,
    class_id: class_id,
    definitions: definitions,
    rules: rules,
  )
}

// Accessors

pub fn class_id(class: Content) -> Int {
  class.class_id
}

pub fn class_name(class: Content) -> String {
  class.class_name
}

pub fn rules(class: Content) -> Option(List(Int)) {
  class.rules
}

pub fn definitions(class: Content) -> List(String) {
  let Definitions(medias, selectors, class) = class.definitions
  [[class], selectors, medias]
  |> list.flatten()
}

// Setters

pub fn set_rules(class: Content, rules: List(Int)) -> Content {
  Content(..class, rules: Some(rules))
}
