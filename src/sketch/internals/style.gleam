import gleam/list
import gleam/option.{None, Some}
import gleam/string
import sketch/internals/string as sketch_string

pub type Style {
  ClassName(class_name: String)
  Media(query: String, styles: List(Style))
  PseudoSelector(pseudo_selector: String, styles: List(Style))
  Property(key: String, value: String, important: Bool)
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

// Computing of properties

/// The Style data structure being a recursive data, computeProperties traverse
/// the data structure and collect the properties with their context.
pub fn compute_properties(
  properties: List(Style),
  indent: Int,
) -> ComputedProperties {
  use acc, prop <- list.fold_right(properties, init_computed_properties(indent))
  case prop {
    ClassName(class_name) -> handle_class_name(acc, class_name)
    Property(_, _, _) -> handle_property(acc, prop)
    Media(_, _) -> handle_media(acc, prop)
    PseudoSelector(_, _) -> handle_pseudo_selector(acc, prop)
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

fn handle_media(props: ComputedProperties, style: Style) {
  let assert Media(query, styles) = style
  let computed_props = compute_properties(styles, props.indent + 2)
  MediaProperty(
    query: query,
    properties: computed_props.properties,
    pseudo_selectors: computed_props.pseudo_selectors,
  )
  |> list.prepend(props.medias, _)
  |> fn(m) { ComputedProperties(..props, medias: m) }
}

fn handle_pseudo_selector(props: ComputedProperties, style: Style) {
  let assert PseudoSelector(pseudo_selector, styles) = style
  let computed_props = compute_properties(styles, props.indent + 2)
  PseudoProperty(
    pseudo_selector: pseudo_selector,
    properties: computed_props.properties,
  )
  |> list.prepend(computed_props.pseudo_selectors, _)
  |> list.append(props.pseudo_selectors)
  |> fn(p) { ComputedProperties(..props, pseudo_selectors: p) }
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
) {
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
