import glance as g
import gleam/list
import gleam/option
import gleam/result
import gleam/string

/// Rewrites every module call from partially qualified to fully qualified.
/// Every `module.function` will be renamed to `fully/qualified/module.function`.
/// Handles plain modules and aliases.
pub fn rewrite(mod: g.Module) -> g.Module {
  let imports = list.map(mod.imports, fn(import_) { import_.definition })
  let functions = list.map(mod.functions, rewrite_function(_, imports))
  let constants = list.map(mod.constants, rewrite_constant(_, imports))
  let custom_types = list.map(mod.custom_types, rewrite_custom_type(_, imports))
  let type_aliases = list.map(mod.type_aliases, rewrite_type_alias(_, imports))
  g.Module(..mod, functions:, constants:, custom_types:, type_aliases:)
}

fn remove_attribute(
  attribute: g.Attribute,
  imports: List(g.Import),
) -> g.Attribute {
  let arguments = list.map(attribute.arguments, rewrite_expr(_, imports))
  g.Attribute(..attribute, arguments:)
}

fn rewrite_function(
  function: g.Definition(g.Function),
  imports: List(g.Import),
) -> g.Definition(g.Function) {
  let g.Definition(attributes:, definition:) = function
  let attributes = list.map(attributes, remove_attribute(_, imports))
  g.Definition(attributes:, definition: {
    let g.Function(parameters:, return:, body:, ..) = definition
    let parameters = list.map(parameters, rewrite_parameters(_, imports))
    let return = option.map(return, rewrite_type(_, imports))
    let body = list.map(body, rewrite_statement(_, imports))
    g.Function(..definition, parameters:, return:, body:)
  })
}

fn rewrite_constant(
  constant: g.Definition(g.Constant),
  imports: List(g.Import),
) -> g.Definition(g.Constant) {
  let g.Definition(attributes:, definition:) = constant
  let attributes = list.map(attributes, remove_attribute(_, imports))
  g.Definition(attributes:, definition: {
    let g.Constant(annotation:, value:, ..) = definition
    let annotation = option.map(annotation, rewrite_type(_, imports))
    let value = rewrite_expr(value, imports)
    g.Constant(..definition, annotation:, value:)
  })
}

fn rewrite_custom_type(
  custom_type: g.Definition(g.CustomType),
  imports: List(g.Import),
) -> g.Definition(g.CustomType) {
  let g.Definition(attributes:, definition:) = custom_type
  let attributes = list.map(attributes, remove_attribute(_, imports))
  g.Definition(attributes:, definition: {
    g.CustomType(..definition, variants: {
      use variant <- list.map(definition.variants)
      g.Variant(..variant, fields: {
        use field <- list.map(variant.fields)
        let item = rewrite_type(field.item, imports)
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
  imports: List(g.Import),
) -> g.Definition(g.TypeAlias) {
  let g.Definition(attributes:, definition:) = type_alias
  let attributes = list.map(attributes, remove_attribute(_, imports))
  g.Definition(attributes:, definition: {
    let aliased = rewrite_type(definition.aliased, imports)
    g.TypeAlias(..definition, aliased:)
  })
}

fn replace_import(name: String, imports: List(g.Import)) -> String {
  let by_alias = fn(i: g.Import) { i.alias == option.Some(g.Named(name)) }
  let by_name = fn(i: g.Import) { string.ends_with(i.module, name) }
  list.find(imports, by_alias)
  |> result.try_recover(fn(_) { list.find(imports, by_name) })
  |> result.map(fn(import_) { import_.module })
  |> result.unwrap(name)
}

fn rewrite_parameters(
  parameter: g.FunctionParameter,
  imports: List(g.Import),
) -> g.FunctionParameter {
  let rewrite_type = rewrite_type(_, imports)
  let type_ = option.map(parameter.type_, rewrite_type)
  g.FunctionParameter(..parameter, type_:)
}

fn rewrite_type(type_: g.Type, imports: List(g.Import)) -> g.Type {
  let rewrite_type = rewrite_type(_, imports)
  case type_ {
    g.TupleType(..) -> {
      let elements = list.map(type_.elements, rewrite_type)
      g.TupleType(elements:)
    }

    g.NamedType(..) -> {
      let module = option.map(type_.module, replace_import(_, imports))
      let parameters = list.map(type_.parameters, rewrite_type)
      g.NamedType(..type_, module:, parameters:)
    }

    g.FunctionType(..) -> {
      let parameters = list.map(type_.parameters, rewrite_type)
      let return = rewrite_type(type_.return)
      g.FunctionType(parameters:, return:)
    }

    _ -> type_
  }
}

fn rewrite_statement(stat: g.Statement, imports: List(g.Import)) -> g.Statement {
  let rewrite_expr = rewrite_expr(_, imports)
  case stat {
    g.Expression(e) -> g.Expression(rewrite_expr(e))
    g.Use(..) -> g.Use(..stat, function: rewrite_expr(stat.function))
    g.Assignment(..) -> {
      let pattern = rewrite_pattern(stat.pattern, imports)
      let annotation = option.map(stat.annotation, rewrite_type(_, imports))
      let value = rewrite_expr(stat.value)
      g.Assignment(..stat, pattern:, annotation:, value:)
    }
  }
}

fn rewrite_pattern(pattern: g.Pattern, imports: List(g.Import)) -> g.Pattern {
  let rewrite_pattern = rewrite_pattern(_, imports)
  case pattern {
    g.PatternTuple(elems:) -> g.PatternTuple(list.map(elems, rewrite_pattern))

    g.PatternList(elements:, tail:) -> {
      let elements = list.map(elements, rewrite_pattern)
      let tail = option.map(tail, rewrite_pattern)
      g.PatternList(elements:, tail:)
    }

    g.PatternBitString(segments:) -> {
      g.PatternBitString(segments: {
        use #(pattern, segments) <- list.map(segments)
        #(rewrite_pattern(pattern), {
          use segment <- list.map(segments)
          case segment {
            g.SizeValueOption(type_) ->
              g.SizeValueOption(rewrite_pattern(type_))
            _ -> segment
          }
        })
      })
    }

    g.PatternConstructor(module:, arguments:, ..) -> {
      let module = option.map(module, replace_import(_, imports))
      g.PatternConstructor(..pattern, module:, arguments: {
        use argument <- list.map(arguments)
        rewrite_field(argument, rewrite_pattern)
      })
    }

    _ -> pattern
  }
}

fn rewrite_field(field: g.Field(a), mapper: fn(a) -> a) -> g.Field(a) {
  case field {
    g.LabelledField(item:, ..) -> g.LabelledField(..field, item: mapper(item))
    g.UnlabelledField(item:) -> g.UnlabelledField(item: mapper(item))
    g.ShorthandField(label:) -> g.ShorthandField(label:)
  }
}

fn rewrite_expr(expr: g.Expression, imports: List(g.Import)) -> g.Expression {
  let rewrite_expr = rewrite_expr(_, imports)
  let rewrite_type = rewrite_type(_, imports)
  let rewrite_statement = rewrite_statement(_, imports)
  let rewrite_pattern = rewrite_pattern(_, imports)
  let rewrite_expr_field = rewrite_field(_, rewrite_expr)
  case expr {
    g.NegateInt(expr) -> g.NegateInt(rewrite_expr(expr))
    g.NegateBool(expr) -> g.NegateBool(rewrite_expr(expr))
    g.Block(stats) -> g.Block(list.map(stats, rewrite_statement))
    g.Panic(expr) -> g.Panic(option.map(expr, rewrite_expr))
    g.Todo(expr) -> g.Todo(option.map(expr, rewrite_expr))
    g.Tuple(exprs) -> g.Tuple(list.map(exprs, rewrite_expr))
    g.TupleIndex(..) -> g.TupleIndex(..expr, tuple: rewrite_expr(expr.tuple))

    g.Fn(return_annotation:, body:, arguments:) -> {
      let return_annotation = option.map(return_annotation, rewrite_type)
      let body = list.map(body, rewrite_statement)
      g.Fn(return_annotation:, body:, arguments: {
        use argument <- list.map(arguments)
        let type_ = option.map(argument.type_, rewrite_type)
        g.FnParameter(..argument, type_:)
      })
    }

    g.List(..) -> {
      let elements = list.map(expr.elements, rewrite_expr)
      let rest = option.map(expr.rest, rewrite_expr)
      g.List(elements:, rest:)
    }

    g.RecordUpdate(..) -> {
      let module = option.map(expr.module, replace_import(_, imports))
      let record = rewrite_expr(expr.record)
      g.RecordUpdate(..expr, module:, record:, fields: {
        use field <- list.map(expr.fields)
        let item = option.map(field.item, rewrite_expr)
        g.RecordUpdateField(..field, item:)
      })
    }

    g.FieldAccess(..) -> {
      let container = rewrite_expr(expr.container)
      g.FieldAccess(..expr, container:)
    }

    g.Call(..) -> {
      let function = rewrite_expr(expr.function)
      let arguments = list.map(expr.arguments, rewrite_expr_field)
      g.Call(function:, arguments:)
    }

    g.FnCapture(function:, arguments_before:, arguments_after:, ..) -> {
      let function = rewrite_expr(function)
      let arguments_before = list.map(arguments_before, rewrite_expr_field)
      let arguments_after = list.map(arguments_after, rewrite_expr_field)
      g.FnCapture(..expr, function:, arguments_before:, arguments_after:)
    }

    g.BitString(segments:) -> {
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
      let subjects = list.map(subjects, rewrite_expr)
      g.Case(subjects:, clauses: {
        use g.Clause(patterns:, guard:, body:) <- list.map(clauses)
        let patterns = list.map(patterns, list.map(_, rewrite_pattern))
        let guard = option.map(guard, rewrite_expr)
        let body = rewrite_expr(body)
        g.Clause(patterns:, guard:, body:)
      })
    }

    g.BinaryOperator(name:, left:, right:) -> {
      let left = rewrite_expr(left)
      let right = rewrite_expr(right)
      g.BinaryOperator(name:, left:, right:)
    }

    _ -> expr
  }
}
