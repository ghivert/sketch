import glance as g
import gleam/list
import gleam/result
import snag

pub fn reject_cycles(
  module: #(String, g.Module),
  visited: List(String),
  modules: List(#(String, g.Module)),
) -> snag.Result(Nil) {
  let modules_ = list.map({ module.1 }.imports, fn(i) { i.definition.module })
  case list.any(modules_, fn(module) { list.contains(visited, module) }) {
    True -> snag.error("cycle detected in module: " <> module.0)
    False -> {
      let by_name = fn(name) { list.find(modules, fn(m) { m.0 == name }) }
      list.filter_map(modules_, by_name)
      |> list.try_map(reject_cycles(_, [module.0, ..visited], modules))
      |> result.replace(Nil)
    }
  }
}

/// True if the first is a dependency of the second.
pub fn is_dependency(
  a: #(String, g.Module),
  of b: #(String, g.Module),
  modules modules: List(#(String, g.Module)),
) -> Bool {
  err_if_dependency(a, b, [], modules)
  |> result.is_error
}

fn err_if_dependency(
  a: #(String, g.Module),
  b: #(String, g.Module),
  visited: List(String),
  modules: List(#(String, g.Module)),
) -> snag.Result(Nil) {
  let modules_ = list.map({ b.1 }.imports, fn(i) { i.definition.module })
  case list.any(modules_, fn(module) { module == a.0 }) {
    True -> snag.error("Nil")
    False -> {
      let by_name = fn(name) { list.find(modules, fn(m) { m.0 == name }) }
      list.filter_map(modules_, by_name)
      |> list.filter(fn(m) { !list.contains(visited, m.0) })
      |> list.try_map(err_if_dependency(a, _, [b.0, ..visited], modules))
      |> result.replace(Nil)
    }
  }
}
