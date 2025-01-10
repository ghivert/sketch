import glance as g
import gleam/bool
import gleam/list
import gleam/option
import gleam/order

pub fn by_dependent(a: g.Function, b: g.Function) -> order.Order {
  case in_statements(b.body, a.name) {
    True -> order.Lt
    False -> order.Gt
  }
}

fn in_statements(statements: List(g.Statement), other: String) {
  use found, statement <- list.fold(statements, False)
  use <- bool.guard(when: found, return: found)
  in_statement(statement, other)
}

fn in_statement(statement: g.Statement, other: String) {
  case statement {
    g.Expression(expression) -> in_expression(expression, other)
    g.Assignment(value:, ..) -> in_expression(value, other)
    g.Use(function:, ..) -> in_expression(function, other)
  }
}

fn in_expressions(expressions: List(g.Expression), other: String) {
  use found, expression <- list.fold(expressions, False)
  use <- bool.guard(when: found, return: found)
  in_expression(expression, other)
}

fn in_expression(expression: g.Expression, other: String) -> Bool {
  case expression {
    g.Variable(var) -> var == other
    g.NegateInt(expr) -> in_expression(expr, other)
    g.NegateBool(expr) -> in_expression(expr, other)
    g.FieldAccess(container:, ..) -> in_expression(container, other)
    g.TupleIndex(tuple:, ..) -> in_expression(tuple, other)
    g.Block(statements) -> in_statements(statements, other)
    g.Tuple(expressions) -> in_expressions(expressions, other)
    g.Fn(body:, ..) -> in_statements(body, other)

    g.List(elements:, rest:) -> {
      rest
      |> option.map(list.prepend(elements, _))
      |> option.unwrap(elements)
      |> in_expressions(other)
    }

    g.Call(function:, arguments:) ->
      arguments
      |> list.filter_map(extract_field)
      |> list.prepend(function)
      |> in_expressions(other)

    g.FnCapture(function:, arguments_before:, arguments_after:, ..) ->
      list.append(arguments_before, arguments_after)
      |> list.filter_map(extract_field)
      |> list.prepend(function)
      |> in_expressions(other)

    g.Case(subjects:, clauses:) -> {
      clauses
      |> list.map(fn(clause) { clause.body })
      |> list.append(subjects, _)
      |> in_expressions(other)
    }

    g.BinaryOperator(left:, right:, ..) -> {
      in_expression(left, other) || in_expression(right, other)
    }

    _ -> False
  }
}

fn extract_field(field: g.Field(a)) {
  case field {
    g.UnlabelledField(expression) -> Ok(expression)
    g.LabelledField(_, expression) -> Ok(expression)
    g.ShorthandField(..) -> Error(Nil)
  }
}
