import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/string
import sketch/internals/class
import sketch/internals/string as sketch_string

@external(erlang, "erlang", "phash2")
@external(javascript, "../../xxhash.ffi.mjs", "xxHash32")
fn xx_hash32(content: String) -> Int

pub type Class {
  Class(string_representation: String, content: List(Style))
}

pub type Cache {
  Ephemeral(cache: Dict(String, #(class.Content, Properties)))
  Persistent(cache: Dict(String, #(class.Content, Properties)), current_id: Int)
}

pub fn render(cache: Cache) -> String {
  dict.values(cache.cache)
  |> list.flat_map(fn(c) { class.definitions(c.0) })
  |> string.join("\n\n")
}

pub fn persistent() -> Cache {
  Persistent(cache: dict.new(), current_id: 0)
}

pub fn ephemeral() -> Cache {
  Ephemeral(cache: dict.new())
}

pub type Style {
  ClassName(class_name: Class)
  Media(query: String, styles: List(Style))
  PseudoSelector(pseudo_selector: String, styles: List(Style))
  Property(key: String, value: String, important: Bool)
  NoStyle
}

pub type Properties {
  Properties(
    properties: List(String),
    medias: List(MediaProperty),
    pseudo_selectors: List(PseudoProperty),
    indentation: Int,
  )
}

pub type MediaProperty {
  MediaProperty(
    query: String,
    properties: List(String),
    pseudo_selectors: List(PseudoProperty),
  )
}

pub type PseudoProperty {
  PseudoProperty(pseudo_selector: String, properties: List(String))
}

// Computing of properties

fn compute_property(
  indent: Int,
  key: String,
  value: String,
  important: Bool,
) -> String {
  let base_indent = sketch_string.indent(indent)
  let important_ = case important {
    True -> " !important"
    False -> ""
  }
  base_indent <> key <> ": " <> value <> important_ <> ";"
}

fn merge_computed_properties(
  target: Properties,
  argument: Properties,
) -> Properties {
  Properties(
    indentation: target.indentation,
    properties: list.append(argument.properties, target.properties),
    medias: list.append(argument.medias, target.medias),
    pseudo_selectors: list.append(
      argument.pseudo_selectors,
      target.pseudo_selectors,
    ),
  )
}

/// The Style data structure being a recursive data, computeProperties traverse
/// the data structure and collect the properties with their context.
pub fn compute_properties(
  cache: Cache,
  properties: List(Style),
  indentation: Int,
) -> #(Cache, Properties) {
  let init =
    Properties(properties: [], medias: [], pseudo_selectors: [], indentation:)
  use #(cache, acc), prop <- list.fold(list.reverse(properties), #(cache, init))
  case prop {
    NoStyle -> #(cache, acc)
    Property(_, _, _) -> #(cache, handle_property(acc, prop))
    Media(_, _) -> handle_media(cache, acc, prop)
    PseudoSelector(_, _) -> handle_pseudo_selector(cache, acc, prop)
    ClassName(class) -> {
      case dict.get(cache.cache, class.string_representation) {
        Ok(#(_, props)) -> #(cache, merge_computed_properties(acc, props))
        Error(_) ->
          compute_properties(cache, class.content, indentation)
          |> pair.map_second(merge_computed_properties(acc, _))
      }
    }
  }
}

fn handle_property(props: Properties, style: Style) -> Properties {
  let assert Property(key:, value:, important:) = style
  let css_property = compute_property(props.indentation, key, value, important)
  let properties = [css_property, ..props.properties]
  Properties(..props, properties:)
}

fn handle_media(
  cache: Cache,
  props: Properties,
  style: Style,
) -> #(Cache, Properties) {
  let assert Media(query:, styles:) = style
  let indentation = props.indentation + 2
  let #(cache, properties) = compute_properties(cache, styles, indentation)
  let Properties(properties:, pseudo_selectors:, ..) = properties
  MediaProperty(query:, properties:, pseudo_selectors:)
  |> list.prepend(props.medias, _)
  |> fn(medias) { Properties(..props, medias:) }
  |> pair.new(cache, _)
}

fn handle_pseudo_selector(
  cache: Cache,
  props: Properties,
  style: Style,
) -> #(Cache, Properties) {
  let assert PseudoSelector(pseudo_selector:, styles:) = style
  let indentation = props.indentation + 2
  let #(cache, properties) = compute_properties(cache, styles, indentation)
  PseudoProperty(pseudo_selector:, properties: properties.properties)
  |> list.prepend(properties.pseudo_selectors, _)
  |> list.append(props.pseudo_selectors)
  |> fn(pseudo_selectors) { Properties(..props, pseudo_selectors:) }
  |> pair.new(cache, _)
}

// Wrapping of classes.

pub type ComputedClass {
  ComputedClass(
    class_def: String,
    medias_def: List(String),
    selectors_def: List(String),
    name: String,
  )
}

fn wrap_pseudo_selectors(
  id: String,
  indentation: Int,
  pseudo_selectors: List(PseudoProperty),
) -> List(String) {
  use pseudo_selector <- list.map(pseudo_selectors)
  let PseudoProperty(pseudo_selector:, properties:) = pseudo_selector
  sketch_string.wrap_class(id, properties, indentation, Some(pseudo_selector))
}

// Compute classes by using the class definitions, and by wrapping them in the
// correct class declarations, to be CSS compliant.
pub fn compute_classes(name: String, properties: Properties) -> ComputedClass {
  let Properties(properties:, medias:, pseudo_selectors:, ..) = properties
  let class_def = sketch_string.wrap_class(name, properties, 0, None)
  let selectors_def = wrap_pseudo_selectors(name, 0, pseudo_selectors)
  ComputedClass(class_def:, selectors_def:, name:, medias_def: {
    use MediaProperty(query, properties, pseudo_selectors) <- list.map(medias)
    let selectors_def = wrap_pseudo_selectors(name, 2, pseudo_selectors)
    [query <> " {", sketch_string.wrap_class(name, properties, 2, None)]
    |> list.prepend([selectors_def, ["}"]], _)
    |> list.flatten()
    |> string.join("\n")
  })
}

pub fn class(content: List(Style)) -> Class {
  let string_representation = string.inspect(content)
  Class(string_representation:, content:)
}

pub fn class_name(class: Class, cache: Cache) -> #(Cache, String) {
  let Class(string_representation:, content:) = class
  use <- bool.guard(when: list.is_empty(content), return: #(cache, ""))
  case dict.get(cache.cache, string_representation) {
    Ok(#(content, _)) -> #(cache, class.class_name(content))
    Error(_) -> compute_class(cache, class) |> pair.map_second(class.class_name)
  }
}

pub fn compute_class(cache: Cache, class: Class) -> #(Cache, class.Content) {
  let Class(string_representation:, content:) = class
  let #(cache, properties) = compute_properties(cache, content, 2)
  let class_id = select_cache_id(cache, string_representation)
  let class_name = "css-" <> int.to_string(class_id)
  let class = compute_classes(class_name, properties)
  let class =
    class.new(
      class_name: class.name,
      class_id:,
      rules: None,
      definitions: class.Definitions(
        medias_def: class.medias_def,
        selectors_def: class.selectors_def,
        class_def: class.class_def,
      ),
    )
  let c = dict.insert(cache.cache, string_representation, #(class, properties))
  case cache {
    Ephemeral(..) -> Ephemeral(c)
    Persistent(..) -> Persistent(cache: c, current_id: class_id + 1)
  }
  |> pair.new(class)
}

fn select_cache_id(cache: Cache, string_representation: String) -> Int {
  case cache {
    Ephemeral(..) -> xx_hash32(string_representation)
    Persistent(current_id:, ..) -> current_id
  }
}
