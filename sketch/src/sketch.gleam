@target(erlang)
import gleam/result
import sketch/css.{type Class}
import sketch/error
@target(erlang)
import sketch/internals/cache/actor
@target(javascript)
import sketch/internals/cache/cache

@target(javascript)
/// Manages the styles. Can be instanciated with [`stylesheet`](#stylesheet).
pub opaque type StyleSheet {
  StyleSheet(cache: cache.Cache, is_persistent: Bool)
}

@target(erlang)
pub opaque type StyleSheet {
  StyleSheet(cache: actor.Cache)
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
/// Convert a `Class` to its class name, to use it anywhere in your application.
/// It always returns the StyleSheet, because the class can have been pushed
/// in the StyleSheet itself.
pub fn class_name(class: Class, stylesheet: StyleSheet) -> #(StyleSheet, String) {
  let #(cache, class_name) = cache.class_name(class, stylesheet.cache)
  #(StyleSheet(..stylesheet, cache:), class_name)
}

@target(erlang)
pub fn class_name(class: Class, stylesheet: StyleSheet) -> #(StyleSheet, String) {
  let #(cache, class_name) = actor.class_name(class, stylesheet.cache)
  #(StyleSheet(cache:), class_name)
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
  Ok(case strategy {
    Ephemeral -> StyleSheet(cache: cache.new(), is_persistent: False)
    Persistent -> StyleSheet(cache: cache.new(), is_persistent: True)
  })
}

@target(erlang)
pub fn stylesheet(
  strategy strategy: Strategy,
) -> Result(StyleSheet, error.SketchError) {
  case strategy {
    Ephemeral -> Ok(actor.ephemeral())
    Persistent -> actor.persistent()
  }
  |> result.map(StyleSheet)
}
