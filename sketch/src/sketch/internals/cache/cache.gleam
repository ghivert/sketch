import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/string
import murmur3a
import sketch/internals/string as sketch_string

pub type Class {
  Class(as_string: String, content: List(Style), name: Option(String))
}

type Definitions {
  Definitions(medias: List(String), selectors: List(String), class: String)
}

type ComputedClass {
  ComputedClass(
    id: String,
    name: String,
    class_name: String,
    definitions: Definitions,
  )
}

pub opaque type Cache {
  Cache(
    cache: Dict(String, #(ComputedClass, Properties)),
    at_rules: Dict(String, String),
  )
}

pub type AtRule {
  AtRule(as_string: String, rule: String, content: AtRuleContent)
}

pub type Global {
  Global(class: Class)
}

pub type AtRuleContent {
  AtRuleClasses(List(Class))
  AtRuleContent(String)
}

pub fn classes_rule(rule: String, content: List(Class)) {
  let as_string = string.inspect(content)
  let content = AtRuleClasses(content)
  AtRule(as_string:, rule:, content:)
}

pub fn content_rule(rule: String, content: String) {
  let as_string = content
  let content = AtRuleContent(content)
  AtRule(as_string:, rule:, content:)
}

pub fn new() {
  Cache(cache: dict.new(), at_rules: dict.new())
}

pub type Style {
  ClassName(class: Class)
  Media(query: String, styles: List(Style))
  Selector(selector: String, styles: List(Style))
  Combinator(selector: String, class: Class, styles: List(Style))
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
  dict.values(cache.at_rules)
  |> list.append({
    dict.values(cache.cache)
    |> list.flat_map(fn(c) { get_definitions(c.0) })
  })
  |> string.join("\n\n")
}

pub fn class(content: List(Style)) -> Class {
  let as_string = string.inspect(content)
  Class(as_string:, content:, name: None)
}

pub fn named(name: String, content: List(Style)) -> Class {
  let as_string = string.inspect(content)
  Class(as_string:, content:, name: Some(name))
}

pub fn class_name(class: Class, cache: Cache) -> #(Cache, String) {
  computed_class(class, cache)
  |> pair.map_second(fn(class) { class.class_name })
}

fn computed_class(class: Class, cache: Cache) -> #(Cache, ComputedClass) {
  use <- bool.lazy_guard(when: list.is_empty(class.content), return: fn() {
    #(cache, empty_computed())
  })
  let existing_class = dict.get(cache.cache, class.as_string)
  case existing_class {
    Ok(#(class, _)) -> #(cache, class)
    Error(..) -> insert_class_in_cache(cache, class)
  }
}

fn empty_computed() {
  let definitions = Definitions([], [], "")
  ComputedClass(id: "", name: "", class_name: "", definitions:)
}

pub fn at_rule(rule: AtRule, cache: Cache) -> Cache {
  case dict.get(cache.at_rules, rule.as_string) {
    Ok(_) -> cache
    Error(_) -> {
      case rule.content {
        AtRuleContent(content) -> "@" <> rule.rule <> " {" <> content <> "}"
        AtRuleClasses(classes) -> {
          list.map(classes, fn(class) {
            let #(_cache, properties) =
              compute_properties(cache, class.content, 2, "")
            let class_ =
              class.as_string
              |> compute_hash
              |> compute_classes(class.name, properties)
            class_.definitions.class
          })
          |> list.map(fn(s) {
            string.split(s, "\n")
            |> list.map(string.append("  ", _))
            |> string.join("\n")
          })
          |> string.join("\n\n")
          |> list.prepend(["}"], _)
          |> list.prepend("@" <> rule.rule <> " {")
          |> string.join("\n")
        }
      }
      |> dict.insert(cache.at_rules, rule.as_string, _)
      |> fn(at_rules) { Cache(..cache, at_rules:) }
    }
  }
}

/// The Style data structure being a recursive data, `compute_properties`
/// traverse the data structure and collect the properties with their context.
fn compute_properties(
  cache: Cache,
  properties: List(Style),
  indentation: Int,
  existing_selector: String,
) -> #(Cache, Properties) {
  let init = Properties(properties: [], medias: [], selectors: [], indentation:)
  use #(cache, acc), p <- list.fold(list.reverse(properties), #(cache, init))
  case p {
    NoStyle -> #(cache, acc)
    Property(..) -> #(cache, handle_property(acc, p))
    Media(..) -> handle_media(cache, acc, p)
    Selector(..) -> handle_selector(cache, acc, p, existing_selector)
    Combinator(..) -> handle_combinator(cache, acc, p, existing_selector)
    ClassName(class) -> {
      case dict.get(cache.cache, class.as_string) {
        Ok(#(_, props)) -> #(cache, merge_computed_properties(acc, props))
        Error(..) ->
          compute_properties(cache, class.content, indentation, "")
          |> pair.map_second(merge_computed_properties(acc, _))
      }
    }
  }
}

// Compute classes by using the class definitions, and by wrapping them in the
// correct class declarations, to be CSS compliant.
fn compute_classes(
  id: String,
  name: Option(String),
  properties: Properties,
) -> ComputedClass {
  let class_name = option.lazy_unwrap(name, fn() { "css-" <> id })
  let name = option.lazy_unwrap(name, fn() { ".css-" <> id })
  let Properties(properties:, medias:, selectors:, ..) = properties
  let class = sketch_string.wrap_class(name, properties, 0, None)
  let selectors = wrap_selectors(name, 0, selectors)
  ComputedClass(id:, name:, class_name:, definitions: {
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

fn insert_class_in_cache(cache: Cache, class: Class) -> #(Cache, ComputedClass) {
  let #(cache, properties) = compute_properties(cache, class.content, 2, "")
  let class_ =
    class.as_string
    |> compute_hash
    |> compute_classes(class.name, properties)
  class_
  |> pair.new(properties)
  |> dict.insert(cache.cache, class.as_string, _)
  |> fn(cache_) { Cache(..cache, cache: cache_) }
  |> pair.new(class_)
}

fn compute_hash(to_hash: String) -> String {
  murmur3a.hash_string(to_hash, 1)
  |> murmur3a.hex_digest
}

fn wrap_selectors(
  id: String,
  indentation: Int,
  selectors: List(SelectorProperty),
) -> List(String) {
  use selector <- list.map(selectors)
  let SelectorProperty(selector:, properties:) = selector
  sketch_string.wrap_class(id, properties, indentation, Some(selector))
}

fn handle_property(props: Properties, property: Style) -> Properties {
  let assert Property(key:, value:, important:) = property
  let css_property = compute_property(props.indentation, key, value, important)
  let properties = [css_property, ..props.properties]
  Properties(..props, properties:)
}

fn handle_media(
  cache: Cache,
  props: Properties,
  media: Style,
) -> #(Cache, Properties) {
  let assert Media(query:, styles:) = media
  let indentation = props.indentation + 2
  let #(cache, properties) = compute_properties(cache, styles, indentation, "")
  let Properties(properties:, selectors:, ..) = properties
  MediaProperty(query:, properties:, selectors:)
  |> list.prepend(props.medias, _)
  |> fn(medias) { Properties(..props, medias:) }
  |> pair.new(cache, _)
}

fn handle_selector(
  cache: Cache,
  props: Properties,
  selector: Style,
  existing_selector: String,
) -> #(Cache, Properties) {
  let assert Selector(selector:, styles:) = selector
  let indentation = props.indentation + 2
  let selector = existing_selector <> selector
  let #(cache, properties) =
    compute_properties(cache, styles, indentation, selector)
  SelectorProperty(selector:, properties: properties.properties)
  |> list.prepend(properties.selectors, _)
  |> list.append(props.selectors)
  |> fn(selectors) { Properties(..props, selectors:) }
  |> pair.new(cache, _)
}

fn handle_combinator(
  cache: Cache,
  props: Properties,
  combinator: Style,
  existing_selector: String,
) -> #(Cache, Properties) {
  let assert Combinator(selector:, class:, styles:) = combinator
  let indentation = props.indentation + 2
  let #(cache, class) = computed_class(class, cache)
  let selector = existing_selector <> selector <> class.name
  let #(cache, properties) =
    compute_properties(cache, styles, indentation, selector)
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
