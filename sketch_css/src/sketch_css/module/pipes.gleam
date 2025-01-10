import glance as g
import gleam/list
import gleam/option

/// Rewrites every pipe (`|>`) to the proper function call. Because pipe is an
/// operator that disappears at runtime, it's useless to keep it in the AST.
pub fn remove(module: g.Module) -> g.Module {
  g.Module(..module, functions: {
    use g.Definition(attributes:, definition:) <- list.map(module.functions)
    let attributes = list.map(attributes, remove_attribute)
    g.Definition(attributes:, definition: {
      g.Function(..definition, body: {
        use statement <- list.map(definition.body)
        remove_statement(statement)
      })
    })
  })
}

fn remove_attribute(attribute: g.Attribute) -> g.Attribute {
  let arguments = list.map(attribute.arguments, remove_expr)
  g.Attribute(..attribute, arguments:)
}

fn remove_statement(stat: g.Statement) -> g.Statement {
  case stat {
    g.Use(..) -> g.Use(..stat, function: remove_expr(stat.function))
    g.Expression(e) -> g.Expression(remove_expr(e))
    g.Assignment(..) -> g.Assignment(..stat, value: remove_expr(stat.value))
  }
}

fn remove_expr(expr: g.Expression) -> g.Expression {
  case expr {
    // Deeply rewrites.
    g.NegateInt(expr) -> g.NegateInt(remove_expr(expr))
    g.NegateBool(expr) -> g.NegateBool(remove_expr(expr))
    g.Block(stat) -> g.Block(list.map(stat, remove_statement))
    g.Todo(stat) -> g.Todo(option.map(stat, remove_expr))
    g.Panic(stat) -> g.Panic(option.map(stat, remove_expr))
    g.Tuple(stat) -> g.Tuple(list.map(stat, remove_expr))
    g.Fn(..) -> g.Fn(..expr, body: list.map(expr.body, remove_statement))
    g.TupleIndex(..) -> g.TupleIndex(..expr, tuple: remove_expr(expr.tuple))

    g.List(elements, rest) -> {
      let elements = list.map(elements, remove_expr)
      let rest = option.map(rest, remove_expr)
      g.List(elements:, rest:)
    }

    g.RecordUpdate(..) -> {
      let record = remove_expr(expr.record)
      g.RecordUpdate(..expr, record:, fields: {
        use field <- list.map(expr.fields)
        let item = option.map(field.item, remove_expr)
        g.RecordUpdateField(..field, item:)
      })
    }

    g.FieldAccess(..) -> {
      let container = remove_expr(expr.container)
      g.FieldAccess(..expr, container:)
    }

    g.Call(..) -> {
      let function = remove_expr(expr.function)
      g.Call(function:, arguments: {
        use argument <- list.map(expr.arguments)
        remove_expr_field(argument)
      })
    }

    g.Case(..) -> {
      let subjects = list.map(expr.subjects, remove_expr)
      g.Case(subjects:, clauses: {
        use clause <- list.map(expr.clauses)
        let guard = option.map(clause.guard, remove_expr)
        let body = remove_expr(clause.body)
        g.Clause(..clause, guard:, body:)
      })
    }

    // Rewrites pipe.
    g.BinaryOperator(g.Pipe, left:, right: g.Call(function:, arguments:)) -> {
      let left = g.UnlabelledField(remove_expr(left))
      let arguments = list.map(arguments, remove_expr_field)
      let arguments = [left, ..arguments]
      g.Call(function:, arguments:)
    }

    g.BinaryOperator(g.Pipe, left:, right: g.Variable(variable)) -> {
      let left = g.UnlabelledField(remove_expr(left))
      let arguments = [left]
      let function = g.Variable(variable)
      g.Call(function:, arguments:)
    }

    g.BinaryOperator(g.Pipe, left:, right:) -> {
      let left = remove_expr(left)
      let right = remove_expr(right)
      g.Call(function: right, arguments: [g.UnlabelledField(left)])
    }

    // Nothing to handle.
    _ -> expr
  }
}

fn remove_expr_field(argument: g.Field(g.Expression)) -> g.Field(g.Expression) {
  case argument {
    g.UnlabelledField(e) -> g.UnlabelledField(remove_expr(e))
    g.ShorthandField(label:) -> g.ShorthandField(label:)
    g.LabelledField(label:, item:) ->
      g.LabelledField(label:, item: remove_expr(item))
  }
}
