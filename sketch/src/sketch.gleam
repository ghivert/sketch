@target(erlang)
import gleam/result
import sketch/css.{type AtRule, type Class}
import sketch/error
@target(erlang)
import sketch/internals/cache/actor
@target(javascript)
import sketch/internals/cache/cache

@target(javascript)
/// Manages the styles. Can be instanciated with [`stylesheet`](#stylesheet).
pub opaque type StyleSheet {
  StyleSheet(cache: cache.Cache, id: Int, is_persistent: Bool)
}

@target(erlang)
pub opaque type StyleSheet {
  StyleSheet(cache: actor.Cache, id: Int)
}

@target(javascript)
/// Render the content in the stylesheet in proper CSS stylesheet.
pub fn render(cache: StyleSheet) -> String {
  cache.render_sheet(cache.cache)
}

@target(erlang)
pub fn render(cache: StyleSheet) -> String {
  actor.render(cache.cache)
}

@target(javascript)
/// Converts a `Class` to its class name, to use it anywhere in your application.
/// It always returns the StyleSheet, because the class can have been pushed
/// in the StyleSheet itself.
pub fn class_name(class: Class, stylesheet: StyleSheet) -> #(StyleSheet, String) {
  let #(cache, class_name) = cache.class_name(class, stylesheet.cache)
  #(StyleSheet(..stylesheet, cache:), class_name)
}

@target(erlang)
pub fn class_name(class: Class, stylesheet: StyleSheet) -> #(StyleSheet, String) {
  let #(cache, class_name) = actor.class_name(class, stylesheet.cache)
  #(StyleSheet(..stylesheet, cache:), class_name)
}

@target(javascript)
/// Pushes an `@rule` in the StyleSheet, to get it bundled in the outputted CSS.
/// It returns the StyleSheet with the rule added.
pub fn at_rule(rule: AtRule, stylesheet: StyleSheet) -> StyleSheet {
  let cache = cache.at_rule(rule, stylesheet.cache)
  StyleSheet(..stylesheet, cache:)
}

@target(erlang)
pub fn at_rule(rule: AtRule, stylesheet: StyleSheet) -> StyleSheet {
  let cache = actor.at_rule(rule, stylesheet.cache)
  StyleSheet(..stylesheet, cache:)
}

@target(javascript)
/// Pushes a global class in the StyleSheet, to get it bundled in the outputted CSS.
/// It returns the StyleSheet with the rule added.
pub fn global(stylesheet: StyleSheet, global: css.Global) -> StyleSheet {
  let #(cache, _) = cache.class_name(global.class, stylesheet.cache)
  StyleSheet(..stylesheet, cache:)
}

@target(erlang)
pub fn global(stylesheet: StyleSheet, global: css.Global) -> StyleSheet {
  let #(cache, _) = actor.class_name(global.class, stylesheet.cache)
  StyleSheet(..stylesheet, cache:)
}

/// Strategy for the StyleSheet. Two strategies are available as of now: ephemeral
/// and persistent. In the first case, the stylesheet is throwable, and it is
/// not expected to be persisted across renders. A class name is always generated
/// according to its content. Two classes with the exact same properties will
/// always have the exact same name. \
/// The second case is designed to be persisted across renders, and uses an
/// actor to save its content on the BEAM.
pub type Strategy {
  Ephemeral
  Persistent
}

@target(javascript)
/// Create a cache, managing the styles. You can instanciate as much cache as
/// you want, if you need to manage different stylesheets.
/// Instanciating an `Ephemeral` _always_ succeed.
pub fn stylesheet(
  strategy strategy: Strategy,
) -> Result(StyleSheet, error.SketchError) {
  let id = unique_id()
  Ok(case strategy {
    Ephemeral -> StyleSheet(cache: cache.new(), id:, is_persistent: False)
    Persistent -> StyleSheet(cache: cache.new(), id:, is_persistent: True)
  })
}

@target(erlang)
pub fn stylesheet(
  strategy strategy: Strategy,
) -> Result(StyleSheet, error.SketchError) {
  let id = unique_id()
  use cache <- result.map(case strategy {
    Ephemeral -> Ok(actor.ephemeral())
    Persistent -> actor.persistent()
  })
  StyleSheet(cache:, id:)
}

@external(erlang, "erlang", "unique_integer")
@external(javascript, "./sketch.ffi.mjs", "uniqueId")
fn unique_id() -> Int
