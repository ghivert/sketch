import glance as g
import gleam/list
import gleam/option.{type Option}
import gleam/pair
import gleam/result

/// Associate exposed name to fully qualified name.
/// [#("class", option.Some("sketch/css", "class"))]
type Environment =
  List(#(String, Option(#(String, String))))

/// Rewrites every exposed types and functions to fully qualified types and functions.
/// Every non-local `function` or `Type` will be rewrote to `fully/qualified/module.function`
/// or `fully/qualified/module.Type`. Handles types, functions and aliases.
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
  let import_ = import_.definition
  let g.Import(unqualified_types:, unqualified_values:, module:, ..) = import_
  list.fold(unqualified_values, env, add_qualified_import(module))
  |> list.fold(unqualified_types, _, add_qualified_import(module))
}

fn add_qualified_import(module: String) {
  fn(env: Environment, import_: g.UnqualifiedImport) -> Environment {
    import_.alias
    |> option.unwrap(import_.name)
    |> pair.new(option.Some(#(module, import_.name)))
    |> list.prepend(env, _)
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

/// Find if `name` is exposed and return its `module` in that case.
fn replace_module(name: String, env: Environment) -> Option(String) {
  list.key_find(env, name)
  |> result.unwrap(option.None)
  |> option.map(pair.first)
}

/// Find if `name` is exposed and return its fully qualified name in that case.
/// Return the original `name` if not found.
fn replace_name(name: String, env: Environment) -> String {
  list.key_find(env, name)
  |> result.unwrap(option.None)
  |> option.map(pair.second)
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
      g.TupleType(..type_, elements:)
    }

    g.NamedType(module: option.None, name:, ..) -> {
      let module = replace_module(name, env)
      let name = replace_name(name, env)
      let parameters = list.map(type_.parameters, rewrite_type(_, env))
      g.NamedType(..type_, module:, parameters:, name:)
    }

    g.FunctionType(..) -> {
      let parameters = list.map(type_.parameters, rewrite_type(_, env))
      let return = rewrite_type(type_.return, env)
      g.FunctionType(..type_, parameters:, return:)
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

    g.Assert(..) -> {
      let expression = rewrite_expr(stat.expression, env)
      let message = option.map(stat.message, rewrite_expr(_, env))
      let stat = g.Assert(..stat, expression:, message:)
      #([stat, ..stats], env)
    }
  }
}

fn rewrite_pattern(
  pattern: g.Pattern,
  env: Environment,
) -> #(g.Pattern, Environment) {
  case pattern {
    g.PatternVariable(name:, ..) -> {
      #(name, option.None)
      |> list.prepend(env, _)
      |> pair.new(g.PatternVariable(..pattern, name:), _)
    }

    g.PatternTuple(elements:, location:) -> {
      rewrite_patterns(elements, env)
      |> pair.map_first(g.PatternTuple(location:, elements: _))
    }

    g.PatternList(elements:, tail:, ..) -> {
      let #(elements, env) = rewrite_patterns(elements, env)
      let #(tail, env) = case tail {
        option.None -> #(option.None, env)
        option.Some(tail) ->
          rewrite_pattern(tail, env)
          |> pair.map_first(option.Some)
      }
      #(g.PatternList(..pattern, elements:, tail:), env)
    }

    g.PatternBitString(segments:, location:) -> {
      list.fold(segments, #([], env), fn(acc, segment) {
        let #(exprs, env) = acc
        let #(pattern, segments) = segment
        let #(pattern, env) = rewrite_pattern(pattern, env)
        let #(segments, env) = rewrite_bitstring_segments_pattern(segments, env)
        #([#(pattern, segments), ..exprs], env)
      })
      |> pair.map_first(list.reverse)
      |> pair.map_first(g.PatternBitString(location:, segments: _))
    }

    g.PatternConcatenate(rest_name: g.Named(name) as rest_name, ..) -> {
      let env = [#(name, option.None), ..env]
      #(g.PatternConcatenate(..pattern, rest_name:), env)
    }

    g.PatternVariant(arguments:, constructor:, ..) -> {
      let module = replace_module(constructor, env)
      let constructor = replace_name(constructor, env)
      let #(arguments, env) = rewrite_pattern_fields(arguments, env)
      #(g.PatternVariant(..pattern, module:, arguments:, constructor:), env)
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
      g.ShorthandField(label:, ..) -> {
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
    g.ShorthandField(label:, ..) -> g.ShorthandField(..field, label:)
    g.LabelledField(item:, ..) -> {
      let item = rewrite_expr(item, env)
      g.LabelledField(..field, item:)
    }
  }
}

fn rewrite_expr(expr: g.Expression, env: Environment) -> g.Expression {
  case expr {
    g.NegateInt(..) -> g.NegateInt(..expr, value: rewrite_expr(expr.value, env))
    g.NegateBool(..) ->
      g.NegateBool(..expr, value: rewrite_expr(expr.value, env))
    g.Panic(..) ->
      g.Panic(..expr, message: option.map(expr.message, rewrite_expr(_, env)))
    g.Todo(..) ->
      g.Todo(..expr, message: option.map(expr.message, rewrite_expr(_, env)))
    g.Tuple(..) ->
      g.Tuple(..expr, elements: list.map(expr.elements, rewrite_expr(_, env)))
    g.TupleIndex(..) ->
      g.TupleIndex(..expr, tuple: rewrite_expr(expr.tuple, env))

    g.Variable(name:, location:) -> {
      list.key_find(env, name)
      |> result.unwrap(option.None)
      |> option.map(fn(found) {
        let #(name, label) = found
        let container = g.Variable(location:, name:)
        g.FieldAccess(location:, container:, label:)
      })
      |> option.unwrap(g.Variable(name:, location:))
    }

    g.Block(statements:, ..) -> {
      g.Block(..expr, statements: {
        list.fold(statements, #([], env), rewrite_statement)
        |> pair.first
        |> list.reverse
      })
    }

    g.Fn(return_annotation:, body:, arguments:, ..) -> {
      let rewrite_type = rewrite_type(_, env)
      let return_annotation = option.map(return_annotation, rewrite_type)
      let body = rewrite_function_body(body, env)
      g.Fn(..expr, return_annotation:, body:, arguments: {
        use argument <- list.map(arguments)
        let type_ = option.map(argument.type_, rewrite_type)
        g.FnParameter(..argument, type_:)
      })
    }

    g.List(..) -> {
      let elements = list.map(expr.elements, rewrite_expr(_, env))
      let rest = option.map(expr.rest, rewrite_expr(_, env))
      g.List(..expr, elements:, rest:)
    }

    g.RecordUpdate(..) -> {
      let record = rewrite_expr(expr.record, env)
      let expr =
        g.RecordUpdate(..expr, record:, fields: {
          use field <- list.map(expr.fields)
          let item = option.map(field.item, rewrite_expr(_, env))
          g.RecordUpdateField(..field, item:)
        })
      case expr.module {
        option.Some(..) -> expr
        option.None -> {
          let constructor = replace_name(expr.constructor, env)
          let module = replace_module(expr.constructor, env)
          g.RecordUpdate(..expr, constructor:, module:)
        }
      }
    }

    g.FieldAccess(..) -> {
      let container = rewrite_expr(expr.container, env)
      g.FieldAccess(..expr, container:)
    }

    g.Call(..) -> {
      let rewrite_expr_field = rewrite_expr_field(_, env)
      let function = rewrite_expr(expr.function, env)
      let arguments = list.map(expr.arguments, rewrite_expr_field)
      g.Call(..expr, function:, arguments:)
    }

    g.FnCapture(function:, arguments_before:, arguments_after:, ..) -> {
      let rewrite_expr_field = rewrite_expr_field(_, env)
      let function = rewrite_expr(function, env)
      let arguments_before = list.map(arguments_before, rewrite_expr_field)
      let arguments_after = list.map(arguments_after, rewrite_expr_field)
      g.FnCapture(..expr, function:, arguments_before:, arguments_after:)
    }

    g.BitString(segments:, ..) -> {
      let rewrite_expr = rewrite_expr(_, env)
      g.BitString(..expr, segments: {
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

    g.Case(subjects:, clauses:, ..) -> {
      let subjects = list.map(subjects, rewrite_expr(_, env))
      g.Case(..expr, subjects:, clauses: {
        use g.Clause(patterns:, guard:, body:) <- list.map(clauses)
        let #(patterns, env) = rewrite_nested_patterns(patterns, env)
        let guard = option.map(guard, rewrite_expr(_, env))
        let body = rewrite_expr(body, env)
        g.Clause(patterns:, guard:, body:)
      })
    }

    g.BinaryOperator(name:, left:, right:, ..) -> {
      let left = rewrite_expr(left, env)
      let right = rewrite_expr(right, env)
      g.BinaryOperator(..expr, name:, left:, right:)
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
