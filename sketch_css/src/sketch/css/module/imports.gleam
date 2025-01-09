import glance as g
import gleam/list
import gleam/option.{type Option}
import gleam/pair
import gleam/result
import gleam/string

type Environment =
  List(#(String, Option(g.Import)))

/// Rewrites every module call from partially qualified to fully qualified.
/// Every `module.function` will be renamed to `fully/qualified/module.function`.
/// Handles plain modules and aliases.
pub fn rewrite(mod: g.Module) -> g.Module {
  let env = init_environment(mod.imports)
  let type_aliases = list.map(mod.type_aliases, rewrite_type_alias(_, env))
  let custom_types = list.map(mod.custom_types, rewrite_custom_type(_, env))
  let constants = list.map(mod.constants, rewrite_constant(_, env))
  let functions = list.map(mod.functions, rewrite_function(_, env))
  g.Module(..mod, functions:, constants:, custom_types:, type_aliases:)
}

fn init_environment(imports: List(g.Definition(g.Import))) -> Environment {
  use env, import_ <- list.fold(imports, [])
  let g.Import(alias:, module:, ..) = import_.definition
  let import_ = option.Some(import_.definition)
  case alias {
    option.Some(g.Named(name)) -> [#(name, import_), ..env]
    option.Some(g.Discarded(..)) -> env
    option.None ->
      module
      |> string.split(on: "/")
      |> list.last
      |> result.map(pair.new(_, import_))
      |> result.map(list.prepend(env, _))
      |> result.unwrap(env)
  }
}

fn rewrite_function(
  function: g.Definition(g.Function),
  env: Environment,
) -> g.Definition(g.Function) {
  let g.Definition(attributes:, definition:) = function
  g.Definition(attributes:, definition: {
    let g.Function(parameters:, return:, body:, ..) = definition
    let parameters = list.map(parameters, rewrite_parameters(_, env))
    let return = option.map(return, rewrite_type(_, env))
    let body = rewrite_function_body(body, env)
    g.Function(..definition, parameters:, return:, body:)
  })
}

fn rewrite_function_body(body: List(g.Statement), env: Environment) {
  list.fold(body, #([], env), rewrite_statement)
  |> pair.first
  |> list.reverse
}

fn rewrite_constant(
  constant: g.Definition(g.Constant),
  env: Environment,
) -> g.Definition(g.Constant) {
  let g.Definition(attributes:, definition:) = constant
  g.Definition(attributes:, definition: {
    let g.Constant(annotation:, value:, ..) = definition
    let annotation = option.map(annotation, rewrite_type(_, env))
    let value = rewrite_expr(value, env)
    g.Constant(..definition, annotation:, value:)
  })
}

fn rewrite_custom_type(
  custom_type: g.Definition(g.CustomType),
  env: Environment,
) -> g.Definition(g.CustomType) {
  let g.Definition(attributes:, definition:) = custom_type
  g.Definition(attributes:, definition: {
    g.CustomType(..definition, variants: {
      use variant <- list.map(definition.variants)
      g.Variant(..variant, fields: {
        use field <- list.map(variant.fields)
        let item = rewrite_type(field.item, env)
        case field {
          g.LabelledVariantField(..) -> g.LabelledVariantField(..field, item:)
          g.UnlabelledVariantField(..) -> g.UnlabelledVariantField(item:)
        }
      })
    })
  })
}

fn rewrite_type_alias(
  type_alias: g.Definition(g.TypeAlias),
  env: Environment,
) -> g.Definition(g.TypeAlias) {
  let g.Definition(attributes:, definition:) = type_alias
  g.Definition(attributes:, definition: {
    let aliased = rewrite_type(definition.aliased, env)
    g.TypeAlias(..definition, aliased:)
  })
}

fn replace_import(name: String, env: Environment) -> String {
  list.key_find(env, name)
  |> result.unwrap(option.None)
  |> option.map(fn(import_) { import_.module })
  |> option.unwrap(name)
}

fn rewrite_parameters(
  parameter: g.FunctionParameter,
  env: Environment,
) -> g.FunctionParameter {
  let rewrite_type = rewrite_type(_, env)
  let type_ = option.map(parameter.type_, rewrite_type)
  g.FunctionParameter(..parameter, type_:)
}

fn rewrite_type(type_: g.Type, env: Environment) -> g.Type {
  case type_ {
    g.TupleType(..) -> {
      let elements = list.map(type_.elements, rewrite_type(_, env))
      g.TupleType(elements:)
    }

    g.NamedType(..) -> {
      let module = option.map(type_.module, replace_import(_, env))
      let parameters = list.map(type_.parameters, rewrite_type(_, env))
      g.NamedType(..type_, module:, parameters:)
    }

    g.FunctionType(..) -> {
      let parameters = list.map(type_.parameters, rewrite_type(_, env))
      let return = rewrite_type(type_.return, env)
      g.FunctionType(parameters:, return:)
    }

    _ -> type_
  }
}

fn rewrite_statement(
  acc: #(List(g.Statement), Environment),
  stat: g.Statement,
) -> #(List(g.Statement), Environment) {
  let #(stats, env) = acc
  case stat {
    g.Expression(expr) -> {
      let expr = rewrite_expr(expr, env)
      let stat = g.Expression(expr)
      #([stat, ..stats], env)
    }

    g.Use(..) -> {
      let function = rewrite_expr(stat.function, env)
      let stat = g.Use(..stat, function:)
      #([stat, ..stats], env)
    }

    g.Assignment(..) -> {
      let #(pattern, env) = rewrite_pattern(stat.pattern, env)
      let annotation = option.map(stat.annotation, rewrite_type(_, env))
      let value = rewrite_expr(stat.value, env)
      let stat = g.Assignment(..stat, pattern:, annotation:, value:)
      #([stat, ..stats], env)
    }
  }
}

fn rewrite_pattern(
  pattern: g.Pattern,
  env: Environment,
) -> #(g.Pattern, Environment) {
  case pattern {
    g.PatternVariable(name:) -> {
      #(name, option.None)
      |> list.prepend(env, _)
      |> pair.new(g.PatternVariable(name:), _)
    }

    g.PatternTuple(elems:) -> {
      rewrite_patterns(elems, env)
      |> pair.map_first(g.PatternTuple)
    }

    g.PatternList(elements:, tail:) -> {
      let #(elements, env) = rewrite_patterns(elements, env)
      let #(tail, env) = case tail {
        option.None -> #(option.None, env)
        option.Some(tail) ->
          rewrite_pattern(tail, env)
          |> pair.map_first(option.Some)
      }
      #(g.PatternList(elements:, tail:), env)
    }

    g.PatternBitString(segments:) -> {
      list.fold(segments, #([], env), fn(acc, segment) {
        let #(exprs, env) = acc
        let #(pattern, segments) = segment
        let #(pattern, env) = rewrite_pattern(pattern, env)
        let #(segments, env) = rewrite_bitstring_segments_pattern(segments, env)
        #([#(pattern, segments), ..exprs], env)
      })
      |> pair.map_first(list.reverse)
      |> pair.map_first(g.PatternBitString)
    }

    g.PatternConcatenate(right: g.Named(name) as right, left:) -> {
      let env = [#(name, option.None), ..env]
      #(g.PatternConcatenate(left:, right:), env)
    }

    g.PatternConstructor(module:, arguments:, ..) -> {
      let module = option.map(module, replace_import(_, env))
      let #(arguments, env) = rewrite_pattern_fields(arguments, env)
      #(g.PatternConstructor(..pattern, module:, arguments:), env)
    }

    _ -> #(pattern, env)
  }
}

fn rewrite_bitstring_segments_pattern(
  segments: List(g.BitStringSegmentOption(g.Pattern)),
  env: Environment,
) -> #(List(g.BitStringSegmentOption(g.Pattern)), Environment) {
  list.fold(segments, #([], env), fn(acc, segment) {
    let #(segments, env) = acc
    case segment {
      g.SizeValueOption(type_) -> {
        let #(type_, env) = rewrite_pattern(type_, env)
        let segment = g.SizeValueOption(type_)
        #([segment, ..segments], env)
      }
      _ -> #([segment, ..segments], env)
    }
  })
  |> pair.map_first(list.reverse)
}

fn rewrite_patterns(elems: List(g.Pattern), env: Environment) {
  list.fold(elems, #([], env), fn(elems, elem) {
    let #(elems, env) = elems
    rewrite_pattern(elem, env)
    |> pair.map_first(list.prepend(elems, _))
  })
  |> pair.map_first(list.reverse)
}

fn rewrite_pattern_fields(
  arguments: List(g.Field(g.Pattern)),
  env: Environment,
) -> #(List(g.Field(g.Pattern)), Environment) {
  list.fold(arguments, #([], env), fn(acc, argument) {
    let #(exprs, env) = acc
    case argument {
      g.ShorthandField(label:) -> {
        #(label, option.None)
        |> list.prepend(env, _)
        |> pair.new([argument, ..exprs], _)
      }

      g.UnlabelledField(item:) -> {
        let #(item, env) = rewrite_pattern(item, env)
        let field = g.UnlabelledField(item)
        #([field, ..exprs], env)
      }

      g.LabelledField(item:, ..) -> {
        let #(item, env) = rewrite_pattern(item, env)
        let field = g.LabelledField(..argument, item:)
        #([field, ..exprs], env)
      }
    }
  })
  |> pair.map_first(list.reverse)
}

fn rewrite_expr_field(
  field: g.Field(g.Expression),
  env: Environment,
) -> g.Field(g.Expression) {
  case field {
    g.UnlabelledField(item:) -> g.UnlabelledField(item: rewrite_expr(item, env))
    g.ShorthandField(label:) -> g.ShorthandField(label:)
    g.LabelledField(item:, ..) -> {
      let item = rewrite_expr(item, env)
      g.LabelledField(..field, item:)
    }
  }
}

fn rewrite_expr(expr: g.Expression, env: Environment) -> g.Expression {
  case expr {
    g.NegateInt(expr) -> g.NegateInt(rewrite_expr(expr, env))
    g.NegateBool(expr) -> g.NegateBool(rewrite_expr(expr, env))
    g.Panic(expr) -> g.Panic(option.map(expr, rewrite_expr(_, env)))
    g.Todo(expr) -> g.Todo(option.map(expr, rewrite_expr(_, env)))
    g.Tuple(exprs) -> g.Tuple(list.map(exprs, rewrite_expr(_, env)))
    g.Variable(var) -> g.Variable(replace_import(var, env))
    g.TupleIndex(..) ->
      g.TupleIndex(..expr, tuple: rewrite_expr(expr.tuple, env))

    g.Block(stats) -> {
      list.fold(stats, #([], env), rewrite_statement)
      |> pair.first
      |> list.reverse
      |> g.Block
    }

    g.Fn(return_annotation:, body:, arguments:) -> {
      let rewrite_type = rewrite_type(_, env)
      let return_annotation = option.map(return_annotation, rewrite_type)
      let body = rewrite_function_body(body, env)
      g.Fn(return_annotation:, body:, arguments: {
        use argument <- list.map(arguments)
        let type_ = option.map(argument.type_, rewrite_type)
        g.FnParameter(..argument, type_:)
      })
    }

    g.List(..) -> {
      let elements = list.map(expr.elements, rewrite_expr(_, env))
      let rest = option.map(expr.rest, rewrite_expr(_, env))
      g.List(elements:, rest:)
    }

    g.RecordUpdate(..) -> {
      let module = option.map(expr.module, replace_import(_, env))
      let record = rewrite_expr(expr.record, env)
      g.RecordUpdate(..expr, module:, record:, fields: {
        use field <- list.map(expr.fields)
        let item = option.map(field.item, rewrite_expr(_, env))
        g.RecordUpdateField(..field, item:)
      })
    }

    g.FieldAccess(..) -> {
      let container = rewrite_expr(expr.container, env)
      g.FieldAccess(..expr, container:)
    }

    g.Call(..) -> {
      let rewrite_expr_field = rewrite_expr_field(_, env)
      let function = rewrite_expr(expr.function, env)
      let arguments = list.map(expr.arguments, rewrite_expr_field)
      g.Call(function:, arguments:)
    }

    g.FnCapture(function:, arguments_before:, arguments_after:, ..) -> {
      let rewrite_expr_field = rewrite_expr_field(_, env)
      let function = rewrite_expr(function, env)
      let arguments_before = list.map(arguments_before, rewrite_expr_field)
      let arguments_after = list.map(arguments_after, rewrite_expr_field)
      g.FnCapture(..expr, function:, arguments_before:, arguments_after:)
    }

    g.BitString(segments:) -> {
      let rewrite_expr = rewrite_expr(_, env)
      g.BitString(segments: {
        use #(expr, segments) <- list.map(segments)
        #(rewrite_expr(expr), {
          use segment <- list.map(segments)
          case segment {
            g.SizeValueOption(expr) -> g.SizeValueOption(rewrite_expr(expr))
            _ -> segment
          }
        })
      })
    }

    g.Case(subjects:, clauses:) -> {
      let subjects = list.map(subjects, rewrite_expr(_, env))
      g.Case(subjects:, clauses: {
        use g.Clause(patterns:, guard:, body:) <- list.map(clauses)
        let #(patterns, env) = rewrite_nested_patterns(patterns, env)
        let guard = option.map(guard, rewrite_expr(_, env))
        let body = rewrite_expr(body, env)
        g.Clause(patterns:, guard:, body:)
      })
    }

    g.BinaryOperator(name:, left:, right:) -> {
      let left = rewrite_expr(left, env)
      let right = rewrite_expr(right, env)
      g.BinaryOperator(name:, left:, right:)
    }

    _ -> expr
  }
}

fn rewrite_nested_patterns(
  patterns: List(List(g.Pattern)),
  env: Environment,
) -> #(List(List(g.Pattern)), Environment) {
  list.fold(patterns, #([], env), fn(acc, pattern) {
    let #(patterns, env) = acc
    rewrite_patterns(pattern, env)
    |> pair.map_first(list.prepend(patterns, _))
  })
  |> pair.map_first(list.reverse)
}
