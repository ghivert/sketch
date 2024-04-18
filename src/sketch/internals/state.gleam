import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleam/set
import sketch/internals/class.{type Class}
import sketch/internals/style

@external(erlang, "erlang", "unique_integer")
fn unique_integer() -> Int

pub type State {
  State(
    memo_cache: Dict(String, Class),
    active_cache: Dict(String, Class),
    passive_cache: Dict(String, Class),
  )
}

pub fn init() {
  State(
    memo_cache: dict.new(),
    active_cache: dict.new(),
    passive_cache: dict.new(),
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

pub fn memo(state: State, class: Class) {
  let id = class.class_id(class)
  state.memo_cache
  |> dict.get(id)
  |> result.replace(state)
  |> result.try_recover(fn(_) {
    let value = dict.get(state.active_cache, id)
    use class <- result.map(value)
    State(
      ..state,
      memo_cache: dict.insert(state.memo_cache, id, class),
      active_cache: dict.delete(state.active_cache, id),
    )
  }// TODO Insert styles here
  )
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
            state
            // TODO insert styles here
          }
        }
      Error(_) ->
        // TODO delete styles here
        state
    }
  })
  |> io.debug()
}
