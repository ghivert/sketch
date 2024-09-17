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
  EphemeralCache(cache: Dict(String, class.Content))
  PersistentCache(cache: Dict(String, class.Content), current_id: Int)
}

pub fn render(cache: Cache) {
  case cache {
    EphemeralCache(cache) -> render_cache_dict(cache)
    PersistentCache(cache, _) -> render_cache_dict(cache)
  }
}

fn render_cache_dict(cache: Dict(String, class.Content)) {
  dict.values(cache)
  |> list.flat_map(class.definitions)
  |> string.join("\n\n")
}

pub fn persistent() {
  PersistentCache(cache: dict.new(), current_id: 0)
}

pub fn ephemeral() {
  EphemeralCache(cache: dict.new())
}

pub type Style {
  ClassName(class_name: Class)
  Media(query: String, styles: List(Style))
  PseudoSelector(pseudo_selector: String, styles: List(Style))
  Property(key: String, value: String, important: Bool)
  NoStyle
}

pub type ComputedProperties {
  ComputedProperties(
    properties: List(String),
    medias: List(MediaProperty),
    classes: List(String),
    pseudo_selectors: List(PseudoProperty),
    indent: Int,
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
//
fn compute_property(indent: Int, key: String, value: String, important: Bool) {
  let base_indent = sketch_string.indent(indent)
  let important_ = case important {
    True -> " !important"
    False -> ""
  }
  base_indent <> key <> ": " <> value <> important_ <> ";"
}

fn init_computed_properties(indent: Int) {
  ComputedProperties(
    properties: [],
    medias: [],
    classes: [],
    pseudo_selectors: [],
    indent: indent,
  )
}

/// The Style data structure being a recursive data, computeProperties traverse
/// the data structure and collect the properties with their context.
pub fn compute_properties(
  cache: Cache,
  properties: List(Style),
  indent: Int,
) -> #(Cache, ComputedProperties) {
  let init = init_computed_properties(indent)
  use #(cache, acc), prop <- list.fold(list.reverse(properties), #(cache, init))
  case prop {
    NoStyle -> #(cache, acc)
    Property(_, _, _) -> #(cache, handle_property(acc, prop))
    Media(_, _) -> handle_media(cache, acc, prop)
    PseudoSelector(_, _) -> handle_pseudo_selector(cache, acc, prop)
    ClassName(class) -> {
      let #(cache, class) = class_name(class, cache)
      #(cache, handle_class_name(acc, class))
    }
  }
}

fn handle_class_name(props: ComputedProperties, class_name: String) {
  let classes = [class_name, ..props.classes]
  ComputedProperties(..props, classes: classes)
}

fn handle_property(props: ComputedProperties, style: Style) {
  let assert Property(key, value, important) = style
  let css_property = compute_property(props.indent, key, value, important)
  let properties = [css_property, ..props.properties]
  ComputedProperties(..props, properties: properties)
}

fn handle_media(cache: Cache, props: ComputedProperties, style: Style) {
  let assert Media(query, styles) = style
  let #(cache, computed_props) =
    compute_properties(cache, styles, props.indent + 2)
  MediaProperty(
    query: query,
    properties: computed_props.properties,
    pseudo_selectors: computed_props.pseudo_selectors,
  )
  |> list.prepend(props.medias, _)
  |> fn(m) { ComputedProperties(..props, medias: m) }
  |> pair.new(cache, _)
}

fn handle_pseudo_selector(cache: Cache, props: ComputedProperties, style: Style) {
  let assert PseudoSelector(pseudo_selector, styles) = style
  let #(cache, computed_props) =
    compute_properties(cache, styles, props.indent + 2)
  PseudoProperty(
    pseudo_selector: pseudo_selector,
    properties: computed_props.properties,
  )
  |> list.prepend(computed_props.pseudo_selectors, _)
  |> list.append(props.pseudo_selectors)
  |> fn(p) { ComputedProperties(..props, pseudo_selectors: p) }
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
  indent: Int,
  pseudo_selectors: List(PseudoProperty),
) {
  use p <- list.map(pseudo_selectors)
  sketch_string.wrap_class(id, p.properties, indent, Some(p.pseudo_selector))
}

// Compute classes by using the class definitions, and by wrapping them in the
// correct class declarations, to be CSS compliant.
pub fn compute_classes(
  class_name: String,
  computed_properties: ComputedProperties,
) -> ComputedClass {
  let ComputedProperties(properties, medias, classes, pseudo_selectors, _) =
    computed_properties
  let class_def = sketch_string.wrap_class(class_name, properties, 0, None)
  let medias_def = {
    use MediaProperty(query, properties, pseudo_selectors) <- list.map(medias)
    let selectors_def = wrap_pseudo_selectors(class_name, 2, pseudo_selectors)
    [query <> " {", sketch_string.wrap_class(class_name, properties, 2, None)]
    |> list.prepend([selectors_def, ["}"]], _)
    |> list.concat()
    |> string.join("\n")
  }
  let selectors_def = wrap_pseudo_selectors(class_name, 0, pseudo_selectors)
  let name = string.trim(string.join(classes, " ") <> " " <> class_name)
  ComputedClass(class_def, medias_def, selectors_def, name)
}

pub fn class(styles: List(Style)) -> Class {
  let string_representation = string.inspect(styles)
  Class(string_representation: string_representation, content: styles)
}

pub fn class_name(class: Class, cache: Cache) -> #(Cache, String) {
  let Class(string_representation: s, content: c) = class
  use <- bool.guard(when: list.is_empty(c), return: #(cache, ""))
  case dict.get(cache.cache, s) {
    Ok(content) -> #(cache, class.class_name(content))
    Error(_) -> compute_class(cache, class) |> pair.map_second(class.class_name)
  }
}

pub fn compute_class(cache: Cache, class: Class) -> #(Cache, class.Content) {
  let Class(string_representation: s, content: c) = class
  let #(cache, properties) = compute_properties(cache, c, 2)
  let class_id = case cache {
    EphemeralCache(_) -> xx_hash32(s)
    PersistentCache(_, cid) -> cid
  }
  let class_name = "css-" <> int.to_string(class_id)
  compute_classes(class_name, properties)
  |> fn(c: ComputedClass) {
    class.create(
      class_name: c.name,
      class_id: class_id,
      rules: None,
      definitions: class.Definitions(
        medias_def: c.medias_def,
        selectors_def: c.selectors_def,
        class_def: c.class_def,
      ),
    )
  }
  |> fn(class) {
    cache.cache
    |> dict.insert(s, class)
    |> fn(c) {
      case cache {
        EphemeralCache(_) -> EphemeralCache(c)
        PersistentCache(_, _) ->
          PersistentCache(cache: c, current_id: class_id + 1)
      }
    }
    |> pair.new(class)
  }
}
