import redraw.{type Component} as react
import redraw/dom/attribute.{type Attribute} as a
import redraw/dom/html
import sketch
import sketch/css.{type Class}
import sketch/redraw/internals/mutable.{type Mutable}
import sketch/redraw/internals/props
import sketch/redraw/internals/styles

/// Cache required to initialise Sketch Redraw. Use it in conjuction with
/// [`provider`](#provider).
pub opaque type Cache {
  Cache(stylesheet: StyleSheet, context: react.Context(StyleSheet))
}

/// Internal type use to easily manage Context content. Sketch Context contains
/// a `StyleSheet`. `render` directly renders the stylesheet correctly in the
/// HTML Style Element.
type StyleSheet {
  StyleSheet(cache: Mutable(sketch.StyleSheet), render: fn() -> Nil)
}

/// Unique name for Sketch Context. Only used across the module.
const context_name = "Sketch:Redraw:Context"

/// Error message used when querying context. Should be used to indicate to the
/// user what should be done before using `sketch_redraw`.
const error_msg = "Sketch Redraw Provider not set. Please, add the provider in your render tree."

/// Creates the Sketch Context and initialises the Sketch cache. Use it in
/// conjuction with [`provider`](#provider) to setup Sketch Redraw, otherwise
/// nothing will work.
///
/// ```gleam
/// import redraw
/// import redraw/dom/client
/// import sketch/redraw as sketch_redraw
///
/// pub fn main() {
///   let app = app()
///   let cache = sketch_redraw.create_cache()
///   let root = client.create_root("root")
///   client.render(root, {
///     redraw.strict_mode([
///       sketch_redraw.provider(cache, [
///         app(),
///       ]),
///     ])
///   })
/// }
/// ```
pub fn create_cache() -> Cache {
  let style = styles.create_node()
  let assert Ok(cache) = sketch.stylesheet(strategy: sketch.Persistent)
  let cache = mutable.from(cache)
  let render = fn() { styles.dump(style, sketch.render(mutable.get(cache))) }
  let stylesheet = StyleSheet(cache:, render:)
  let assert Ok(context) = react.create_context(context_name, stylesheet)
  Cache(stylesheet:, context:)
}

/// Creates the Sketch Context and initialises the Sketch cache. Use it in
/// conjuction with [`provider`](#provider) to setup Sketch Redraw, otherwise
/// nothing will work. `initialise_cache` acts like `create_cache`, but provides
/// a customisation function to modify the stylesheet at initialisation (to
/// inject custom styles, keyframes, etc.).
///
/// ```gleam
/// import redraw
/// import redraw/dom/client
/// import sketch/redraw as sketch_redraw
///
/// pub fn main() {
///   let app = app()
///   let cache = initialise_cache()
///   let root = client.create_root("root")
///   client.render(root, {
///     redraw.strict_mode([
///       sketch_redraw.provider(cache, [
///         app(),
///       ]),
///     ])
///   })
/// }
///
/// pub fn initialise_cache() {
///   use stylesheet <- sketch_redraw.initialise_cache()
///   stylesheet
///   |> sketch.at_rule(keyframes.wave(), _)
///   |> sketch.at_rule(keyframes.pulse(), _)
/// }
/// ```
pub fn initialise_cache(
  prepare: fn(sketch.StyleSheet) -> sketch.StyleSheet,
) -> Cache {
  let style = styles.create_node()
  let assert Ok(cache) = sketch.stylesheet(strategy: sketch.Persistent)
  let cache = prepare(cache)
  let cache = mutable.from(cache)
  let render = fn() { styles.dump(style, sketch.render(mutable.get(cache))) }
  let stylesheet = StyleSheet(cache:, render:)
  let assert Ok(context) = react.create_context(context_name, stylesheet)
  Cache(stylesheet:, context:)
}

/// Create the Sketch provider used to manage the `StyleSheet`. \
/// This makes sure identical styles will never be computed twice. \
/// Use it at root of your render function.
///
/// ```gleam
/// import redraw
/// import redraw/dom/client
/// import sketch/redraw as sketch_redraw
///
/// pub fn main() {
///   let app = app()
///   let cache = sketch_redraw.create_cache()
///   let root = client.create_root("root")
///   client.render(root, {
///     redraw.strict_mode([
///       sketch_redraw.provider(cache, [
///         app(),
///       ]),
///     ])
///   })
/// }
/// ```
pub fn provider(setup: Cache, children: List(Component)) -> Component {
  let Cache(context:, stylesheet:) = setup
  react.provider(context, stylesheet, children)
}

/// Generates the corresponding class name to `styles`.
///
/// ```gleam
/// import redraw
/// import redraw/dom/attribute
/// import redraw/dom/client
/// import redraw/dom/html
/// import sketch/redraw as sketch_redraw
///
/// pub fn my_component() {
///   use <- redraw.standalone("MyComponent")
///   let class_name = sketch_redraw.use_class_name(my_class())
///   html.div([attribute.class_name(class_name)], [
///     html.text("Styled!"),
///   ])
/// }
/// ```
pub fn use_class_name(styles: Class) -> String {
  let stylesheet = use_sketch_context()
  let class_name = use_styles(stylesheet.cache, styles)
  use_render(stylesheet, class_name)
  react.use_memo(fn() { class_name }, #(class_name))
}

/// Style a native DOM node. `styled` creates an intermediate component named
/// `Sketch.Styled(tag)` which will render the styles in the StyleSheet injected
/// in Context, and inject the class name directly on the node. Every other
/// props are kept as-is.
///
/// ```gleam
/// import redraw.{type Component}
/// import redraw/dom/attribute.{type Attribute}
/// import sketch/css.{type Class}
///
/// pub fn my_node(
///   styles: Class,
///   props: List(Attribute),
///   children: List(Component),
/// ) -> Component {
///   styled("my_node", styles, props, children)
/// }
/// ```
@internal
pub fn styled(
  tag: String,
  styles: Class,
  props: List(Attribute),
  children: a,
) -> Component {
  let as_ = a.attribute("as", tag)
  let styles = a.attribute("styles", styles)
  let fun = styles.cache(tag, factory)
  let props = html.to_props([as_, styles, ..props])
  react.jsx(fun, props, children, convert_children: True)
}

/// React Component rendering a styled HTML element. Those components are
/// created lazily on-demand, and are named correctly according to the `tag`.
fn factory(props: props) -> Component {
  let #(tag, styles, props) = styles.extract_from(props)
  let class_name = use_class_name(styles)
  let props = props.append(props, "className", class_name)
  react.jsx(tag, props, Nil, convert_children: False)
}

/// Style a native DOM node. `hooked` creates an intermediate component named
/// `Sketch.Styled(tag)` which will render the styles in the StyleSheet injected
/// in Context, and inject the class name directly on the node. Every other
/// props are kept as-is.
///
/// `styles` is a function that runs at top-level of the component, allowing
/// to use hooks directly in the function.
///
/// ```gleam
/// import redraw.{type Component}
/// import redraw/dom/attribute.{type Attribute}
/// import sketch/css.{type Class}
///
/// pub fn my_node(
///   styles: Class,
///   props: List(Attribute),
///   children: List(Component),
/// ) -> Component {
///   use <- hooked("my_node", props, children)
///   redraw.use_memo(fn () { styles }, #(styles))
/// }
/// ```
@internal
pub fn hooked(
  tag: String,
  props: List(Attribute),
  children: a,
  styles: fn() -> Class,
) -> Component {
  let as_ = a.attribute("as", tag)
  let styles = a.attribute("styles", styles)
  let fun = styles.hook_cache(tag, hook_factory)
  let props = html.to_props([as_, styles, ..props])
  react.jsx(fun, props, children, convert_children: True)
}

/// React Component rendering a styled HTML element. Those components are
/// created lazily on-demand, and are named correctly according to the `tag`.
fn hook_factory(props: props) -> Component {
  let #(tag, use_user_styles, props) = styles.hook_extract_from(props)
  let styles = use_user_styles()
  let class_name = use_class_name(styles)
  let props = props.append(props, "className", class_name)
  react.jsx(tag, props, Nil, convert_children: False)
}

fn use_sketch_context() -> StyleSheet {
  case react.get_context(context_name) {
    Ok(context) -> react.use_context(context)
    Error(_) -> panic as error_msg
  }
}

fn use_styles(cache: Mutable(sketch.StyleSheet), styles: Class) -> String {
  use <- react.use_memo(_, #(cache, styles.as_string))
  let stylesheet = mutable.get(cache)
  let #(cache_, class_name) = sketch.class_name(styles, stylesheet)
  mutable.set(cache, cache_)
  class_name
}

fn use_render(stylesheet: StyleSheet, class_name: String) -> Nil {
  use <- react.use_insertion_effect(_, #(class_name))
  stylesheet.render()
}
