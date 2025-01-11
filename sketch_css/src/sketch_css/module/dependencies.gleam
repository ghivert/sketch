import glance as g
import gleam/list
import gleam/result
import gleam/string
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

pub fn all_imports(
  module: #(String, g.Module),
  modules: List(#(String, g.Module)),
  visited: List(String),
) -> List(String) {
  let imports = list.map({ module.1 }.imports, fn(i) { i.definition.module })
  let visited = [module.0, ..visited]
  let modules_ =
    list.filter_map(imports, fn(i) { list.find(modules, fn(m) { m.0 == i }) })
  list.flat_map(modules_, all_imports(_, modules, visited))
  |> list.append(imports)
  |> list.unique
  |> list.sort(string.compare)
}
