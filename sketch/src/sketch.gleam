@target(erlang)
import gleam/result
import sketch/css.{type Class}
import sketch/error
@target(erlang)
import sketch/internals/cache/actor
@target(javascript)
import sketch/internals/cache/cache

@target(javascript)
/// Manages the styles. Can be instanciated with [`cache`](#cache).
pub opaque type StyleSheet {
  StyleSheet(cache: cache.Cache, is_persistent: Bool)
}

@target(erlang)
pub opaque type StyleSheet {
  StyleSheet(cache: actor.Cache)
}

@target(javascript)
/// Render the content in the cache in proper CSS stylesheet.
pub fn render(cache: StyleSheet) -> String {
  cache.render_sheet(cache.cache)
}

@target(erlang)
pub fn render(cache: StyleSheet) -> String {
  actor.render(cache.cache)
}

@target(javascript)
/// Convert a `Class` to its proper class name, to use it anywhere in your
/// application. It can have the form `class1` or `class1 class2` in case of
/// classes composition.
pub fn class_name(class: Class, stylesheet: StyleSheet) -> #(StyleSheet, String) {
  let #(cache, class_name) = cache.class_name(class, stylesheet.cache)
  #(StyleSheet(..stylesheet, cache:), class_name)
}

@target(erlang)
pub fn class_name(class: Class, stylesheet: StyleSheet) -> #(StyleSheet, String) {
  let #(cache, class_name) = actor.class_name(class, stylesheet.cache)
  #(StyleSheet(cache:), class_name)
}

/// Strategy for the Cache. Two strategies are available as of now: ephemeral
/// and persistent. In the first case, the cache is throwable, and every class
/// generation wil rely on hashing function. It means two class names will be
/// identical if their content are identical.
/// In the second case, the cache is persistent, meaning it will keep the
/// memories of the generated classes.
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
