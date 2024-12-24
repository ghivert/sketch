import gleam/pair
@target(erlang)
import gleam/result
import sketch/css.{type Class}
import sketch/error
@target(erlang)
import sketch/internals/cache/setup as cache
@target(javascript)
import sketch/internals/style

@target(javascript)
/// Manages the styles. Can be instanciated with [`cache`](#cache).
pub opaque type Cache {
  Cache(cache: style.Cache)
}

@target(erlang)
pub opaque type Cache {
  Cache(cache: cache.Cache)
}

@target(javascript)
/// Render the content in the cache in proper CSS stylesheet.
pub fn render(cache: Cache) -> String {
  style.render(cache.cache)
}

@target(erlang)
pub fn render(cache: Cache) -> String {
  cache.render(cache.cache)
}

@target(javascript)
/// Convert a `Class` to its proper class name, to use it anywhere in your
/// application. It can have the form `class1` or `class1 class2` in case of
/// classes composition.
pub fn class_name(class: Class, cache: Cache) -> #(Cache, String) {
  style.class_name(class, cache.cache)
  |> pair.map_first(Cache)
}

@target(erlang)
pub fn class_name(class: Class, cache: Cache) -> #(Cache, String) {
  cache.class_name(class, cache.cache)
  |> pair.map_first(Cache)
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
pub fn cache(strategy strategy: Strategy) -> Result(Cache, error.SketchError) {
  case strategy {
    Ephemeral -> style.ephemeral()
    Persistent -> style.persistent()
  }
  |> Cache
  |> Ok
}

@target(erlang)
pub fn cache(strategy strategy: Strategy) -> Result(Cache, error.SketchError) {
  case strategy {
    Ephemeral -> Ok(cache.ephemeral())
    Persistent -> cache.persistent()
  }
  |> result.map(Cache)
}
