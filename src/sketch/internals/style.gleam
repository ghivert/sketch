import gleam/list
import sketch/internals/string as sketch_string

pub type Style {
  ClassName(class_name: String)
  Media(query: String, styles: List(Style))
  PseudoSelector(pseudo_selector: String, styles: List(Style))
  Property(key: String, value: String, important: Bool)
}

type ComputedProperties {
  ComputedProperties(
    properties: List(String),
    medias: List(MediaProperty),
    classes: List(String),
    pseudo_selectors: List(PseudoProperty),
    indent: Int,
  )
}

type MediaProperty {
  MediaProperty(
    query: String,
    properties: List(String),
    pseudo_selectors: List(PseudoProperty),
  )
}

type PseudoProperty {
  PseudoProperty(pseudo_selector: String, properties: List(String))
}

fn compute_property(indent: Int, key: String, value: String, important: Bool) {
  let base_indent = sketch_string.indent(indent)
  let important_ = case important {
    True -> " !important"
    False -> ""
  }
  base_indent <> key <> ": " <> value <> important_
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

// The Style data structure being a recursive data, computeProperties traverse
// the data structure and collect the properties with their context.
fn compute_properties(
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
