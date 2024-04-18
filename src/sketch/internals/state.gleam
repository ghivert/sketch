//// BEAM only

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleam/set
import sketch/internals/class.{type Class}
import sketch/internals/style
import sketch/internals/stylesheet

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int

pub type State {
  State(
    memo_cache: Dict(String, Class),
    active_cache: Dict(String, Class),
    passive_cache: Dict(String, Class),
    stylesheet: stylesheet.StyleSheet,
  )
}

pub fn init() {
  State(
    memo_cache: dict.new(),
    active_cache: dict.new(),
    passive_cache: dict.new(),
    stylesheet: stylesheet.init(),
  )
}

pub fn prepare(state: State) {
  State(..state, passive_cache: state.active_cache, active_cache: dict.new())
}

pub fn persist(
  state: State,
  id: String,
  styles: List(style.Style),
) -> Result(#(State, Class), Nil) {
  let value = {
    let memoized_content = dict.get(state.memo_cache, id)
    use <- result.lazy_or(memoized_content)
    let new_content = dict.get(state.active_cache, id)
    use <- result.lazy_or(new_content)
    dict.get(state.passive_cache, id)
  }
  use content <- result.try(value)
  case class.previous_styles(content) == styles {
    False -> Error(Nil)
    True ->
      state.active_cache
      |> dict.insert(id, content)
      |> fn(s) { State(..state, active_cache: s) }
      |> pair.new(content)
      |> Ok
  }
}

pub fn compute_class(
  state: State,
  class_id: String,
  styles: List(style.Style),
) -> #(State, Class) {
  let class_name = "ccs-" <> int.to_string(unique_integer())
  style.compute_properties(styles, 2)
  |> style.compute_classes(class_name, _)
  |> fn(c: style.ComputedClass) {
    class.create(
      class_name: c.name,
      class_id: class_id,
      rules: None,
      previous_styles: styles,
      definitions: class.Definitions(
        medias_def: c.medias_def,
        selectors_def: c.selectors_def,
        class_def: c.class_def,
      ),
    )
  }
  |> fn(class) {
    state.active_cache
    |> dict.insert(class_id, class)
    |> fn(c) { State(..state, active_cache: c) }
    |> pair.new(class)
  }
}

fn insert_rules(class: Class, stylesheet: stylesheet.StyleSheet) {
  case class.rules(class) {
    Some(_) -> #(stylesheet, class)
    None -> {
      class
      |> class.definitions()
      |> list.fold(#(stylesheet, []), fn(acc, def) {
        let #(stylesheet, rules) = acc
        let #(st, index) = stylesheet.insert_styles(stylesheet, def)
        #(st, [index, ..rules])
      })
      |> pair.map_second(class.set_rules(class, _))
    }
  }
}

pub fn memo(state: State, class: Class) {
  let id = class.class_id(class)
  state.memo_cache
  |> dict.get(id)
  |> result.replace(state)
  |> result.try_recover(fn(_) {
    let value = dict.get(state.active_cache, id)
    use class <- result.map(value)
    let #(new_stylesheet, new_class) = insert_rules(class, state.stylesheet)
    State(
      ..state,
      stylesheet: new_stylesheet,
      memo_cache: dict.insert(state.memo_cache, id, new_class),
      active_cache: dict.delete(state.active_cache, id),
    )
  })
  |> result.replace_error(state)
  |> result.unwrap_both()
}

pub fn diff(state: State) {
  set.new()
  |> dict.fold(state.active_cache, _, fn(s, key, _) { set.insert(s, key) })
  |> dict.fold(state.passive_cache, _, fn(s, key, _) { set.insert(s, key) })
  |> set.fold(state, fn(state, key) {
    case dict.get(state.active_cache, key) {
      Ok(class) ->
        case class.rules(class) {
          Some(_) -> state
          None -> {
            let #(st, new_class) = insert_rules(class, state.stylesheet)
            dict.insert(state.active_cache, key, new_class)
            |> fn(c) { State(..state, stylesheet: st, active_cache: c) }
          }
        }
      Error(_) ->
        case dict.get(state.passive_cache, key) {
          Error(_) -> state
          Ok(class) ->
            class.rules(class)
            |> option.unwrap([])
            |> list.fold(state.stylesheet, stylesheet.delete_styles)
            |> fn(st) { State(..state, stylesheet: st) }
        }
    }
  })
}

pub fn render(state: State) {
  stylesheet.render(state.stylesheet)
}
