import redraw.{type Component} as react
import redraw/attribute.{type Attribute} as a
import redraw/internals/attribute
import sketch.{type Class}
import sketch/redraw/internals/mutable as mut
import sketch/redraw/internals/object

type StyleSheet {
  StyleSheet(cache: mut.Mutable(sketch.Cache), render: fn() -> Nil)
}

/// Unique name for Sketch Context. Only used across the module.
const context_name = "SketchRedrawContext"

/// Error message used when querying context. Should be used to indicate to the
/// user what should be done before using `sketch_redraw`.
const error_msg = "Sketch Redraw Provider not set. Please, add the provider in your render tree."

/// Create the Sketch provider used to manage `Cache`. This makes sure identical
/// styles will never be computed twice.
///
/// Use it at root of your render function.
///
/// ```gleam
/// import redraw
/// import redraw_dom/client
/// import sketch/redraw as sketch_redraw
///
/// pub fn main() {
///   let app = app()
///   let root = client.create_root("root")
///   client.render(root, {
///     redraw.strict_mode([
///       sketch_redraw.provider([
///         app(),
///       ]),
///     ])
///   })
/// }
/// ```
pub fn provider(children) {
  let assert Ok(cache) = sketch.cache(strategy: sketch.Ephemeral)
  let cache = mut.wrap(cache)
  let stylesheet = StyleSheet(cache:, render: fn() { Nil })
  let assert Ok(context) = react.create_context_(context_name, stylesheet)
  let style = create_style_tag()
  let render = fn() { dump_styles(style, sketch.render(mut.get(cache))) }
  let stylesheet = StyleSheet(cache:, render:)
  react.provider(context, stylesheet, children)
}

fn get_context() {
  case react.get_context(context_name) {
    Ok(context) -> context
    Error(_) -> panic as error_msg
  }
}

fn generate_class_name(cache, styles) {
  let #(cache_, class_name) = sketch.class_name(styles, mut.get(cache))
  mut.set(cache, cache_)
  class_name
}

fn do_styled(props) {
  let context = get_context()
  let StyleSheet(cache:, render:) = react.use_context(context)
  let #(tag, styles, props) = extract(props)
  let str = styles.string_representation
  let class_name =
    react.use_memo(fn() { generate_class_name(cache, styles) }, #(cache, str))
  use_insertion_effect(fn() { render() }, #(class_name))
  react.jsx(tag, object.add(props, "className", class_name), Nil)
}

/// Style a native DOM node. Can probably be used for custom elements, but props
/// will be different, so I don't know yet how to do it properly.
@internal
pub fn styled(
  tag: String,
  styles: Class,
  props: List(Attribute),
  children: List(Component),
) {
  let as_ = a.attribute("as", tag)
  let styles = a.attribute("styles", styles)
  let fun = styled_fn(tag, do_styled)
  attribute.to_props([as_, styles, ..props])
  |> react.jsx(fun, _, children)
}

// FFI

/// Extract the props generated from `styled` function.
/// Extract `as` and `styles`, and the new props without them.
@external(javascript, "../redraw.ffi.mjs", "extract")
fn extract(props: a) -> #(String, Class, a)

@external(javascript, "../redraw.ffi.mjs", "useInsertionEffect")
fn use_insertion_effect(setup: fn() -> Nil, deps: a) -> Nil

@external(javascript, "../redraw.ffi.mjs", "createStyleTag")
fn create_style_tag() -> a

@external(javascript, "../redraw.ffi.mjs", "dumpStyles")
fn dump_styles(style: a, content: String) -> Nil

/// Creates the styled function from `do_styled` if it does not exists.
/// Otherwise, returns the existing `do_styled` function specialized for the tag.
@external(javascript, "../redraw.ffi.mjs", "styledFn")
fn styled_fn(tag: String, value: a) -> a
