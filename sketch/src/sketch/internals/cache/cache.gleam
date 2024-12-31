import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/string
import sketch/internals/string as sketch_string

pub type Class {
  Class(as_string: String, content: List(Style))
}

type Definitions {
  Definitions(medias: List(String), selectors: List(String), class: String)
}

type ComputedClass {
  ComputedClass(id: Int, name: String, definitions: Definitions)
}

pub opaque type Cache {
  Cache(cache: Dict(String, #(ComputedClass, Properties)))
}

pub fn new() {
  Cache(dict.new())
}

pub type Style {
  ClassName(class_name: Class)
  Media(query: String, styles: List(Style))
  Selector(selector: String, styles: List(Style))
  Property(key: String, value: String, important: Bool)
  NoStyle
}

type Properties {
  Properties(
    properties: List(String),
    medias: List(MediaProperty),
    selectors: List(SelectorProperty),
    indentation: Int,
  )
}

type MediaProperty {
  MediaProperty(
    query: String,
    properties: List(String),
    selectors: List(SelectorProperty),
  )
}

type SelectorProperty {
  SelectorProperty(selector: String, properties: List(String))
}

/// Render a valid CSS StyleSheet from a cache.
pub fn render_sheet(cache: Cache) -> String {
  dict.values(cache.cache)
  |> list.flat_map(fn(c) { get_definitions(c.0) })
  |> string.join("\n\n")
}

pub fn class(content: List(Style)) -> Class {
  let as_string = string.inspect(content)
  Class(as_string:, content:)
}

pub fn class_name(class: Class, cache: Cache) -> #(Cache, String) {
  use <- bool.guard(when: list.is_empty(class.content), return: #(cache, ""))
  let existing_class = dict.get(cache.cache, class.as_string)
  case existing_class {
    Ok(#(class, _)) -> #(cache, class.name)
    Error(..) -> insert_class_in_cache(cache, class)
  }
}

/// The Style data structure being a recursive data, `compute_properties`
/// traverse the data structure and collect the properties with their context.
fn compute_properties(
  cache: Cache,
  properties: List(Style),
  indentation: Int,
) -> #(Cache, Properties) {
  let init = Properties(properties: [], medias: [], selectors: [], indentation:)
  use #(cache, acc), p <- list.fold(list.reverse(properties), #(cache, init))
  case p {
    NoStyle -> #(cache, acc)
    Property(..) -> #(cache, handle_property(acc, p.key, p.value, p.important))
    Media(..) -> handle_media(cache, acc, p.query, p.styles)
    Selector(..) -> handle_selector(cache, acc, p.selector, p.styles)
    ClassName(class) -> {
      case dict.get(cache.cache, class.as_string) {
        Ok(#(_, props)) -> #(cache, merge_computed_properties(acc, props))
        Error(..) ->
          compute_properties(cache, class.content, indentation)
          |> pair.map_second(merge_computed_properties(acc, _))
      }
    }
  }
}

// Compute classes by using the class definitions, and by wrapping them in the
// correct class declarations, to be CSS compliant.
fn compute_classes(id: Int, properties: Properties) -> ComputedClass {
  let name = "css-" <> int.to_string(id)
  let Properties(properties:, medias:, selectors:, ..) = properties
  let class = sketch_string.wrap_class(name, properties, 0, None)
  let selectors = wrap_selectors(name, 0, selectors)
  ComputedClass(id:, name:, definitions: {
    Definitions(class:, selectors:, medias: {
      use MediaProperty(query, properties, selectors) <- list.map(medias)
      let selectors = wrap_selectors(name, 2, selectors)
      [query <> " {", sketch_string.wrap_class(name, properties, 2, None)]
      |> list.prepend([selectors, ["}"]], _)
      |> list.flatten()
      |> string.join("\n")
    })
  })
}

fn insert_class_in_cache(cache: Cache, class: Class) -> #(Cache, String) {
  let #(cache, properties) = compute_properties(cache, class.content, 2)
  let class_ =
    class.as_string
    |> compute_hash
    |> compute_classes(properties)
  class_
  |> pair.new(properties)
  |> dict.insert(cache.cache, class.as_string, _)
  |> Cache
  |> pair.new(class_.name)
}

@external(erlang, "erlang", "phash2")
@external(javascript, "../../xxhash.ffi.mjs", "xxHash32")
fn compute_hash(content: String) -> Int

fn wrap_selectors(
  id: String,
  indentation: Int,
  selectors: List(SelectorProperty),
) -> List(String) {
  use selector <- list.map(selectors)
  let SelectorProperty(selector:, properties:) = selector
  sketch_string.wrap_class(id, properties, indentation, Some(selector))
}

fn handle_property(
  props: Properties,
  key: String,
  value: String,
  important: Bool,
) -> Properties {
  let css_property = compute_property(props.indentation, key, value, important)
  let properties = [css_property, ..props.properties]
  Properties(..props, properties:)
}

fn handle_media(
  cache: Cache,
  props: Properties,
  query: String,
  styles: List(Style),
) -> #(Cache, Properties) {
  let indentation = props.indentation + 2
  let #(cache, properties) = compute_properties(cache, styles, indentation)
  let Properties(properties:, selectors:, ..) = properties
  MediaProperty(query:, properties:, selectors:)
  |> list.prepend(props.medias, _)
  |> fn(medias) { Properties(..props, medias:) }
  |> pair.new(cache, _)
}

fn handle_selector(
  cache: Cache,
  props: Properties,
  selector: String,
  styles: List(Style),
) -> #(Cache, Properties) {
  let indentation = props.indentation + 2
  let #(cache, properties) = compute_properties(cache, styles, indentation)
  SelectorProperty(selector:, properties: properties.properties)
  |> list.prepend(properties.selectors, _)
  |> list.append(props.selectors)
  |> fn(selectors) { Properties(..props, selectors:) }
  |> pair.new(cache, _)
}

fn compute_property(
  indent: Int,
  key: String,
  value: String,
  important: Bool,
) -> String {
  let base_indent = sketch_string.indent(indent)
  let important = case important {
    True -> " !important"
    False -> ""
  }
  base_indent <> key <> ": " <> value <> important <> ";"
}

fn merge_computed_properties(
  target: Properties,
  argument: Properties,
) -> Properties {
  Properties(
    indentation: target.indentation,
    properties: list.append(argument.properties, target.properties),
    medias: list.append(argument.medias, target.medias),
    selectors: list.append(argument.selectors, target.selectors),
  )
}

fn get_definitions(class: ComputedClass) -> List(String) {
  let Definitions(medias, selectors, class) = class.definitions
  [[class], selectors, medias]
  |> list.flatten()
}
