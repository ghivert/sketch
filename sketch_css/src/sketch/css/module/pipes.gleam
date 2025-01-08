import glance as g
import gleam/list
import gleam/option

pub fn remove(module: g.Module) {
  g.Module(..module, functions: {
    use function <- list.map(module.functions)
    g.Definition(..function, definition: {
      g.Function(..function.definition, body: {
        use statement <- list.map(function.definition.body)
        remove_statement(statement)
      })
    })
  })
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
        case argument {
          g.UnlabelledField(e) -> g.UnlabelledField(remove_expr(e))
          g.ShorthandField(label:) -> g.ShorthandField(label:)
          g.LabelledField(label:, item:) ->
            g.LabelledField(label:, item: remove_expr(item))
        }
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
      let left = g.UnlabelledField(left)
      let arguments = [left, ..arguments]
      g.Call(function:, arguments:)
    }

    g.BinaryOperator(g.Pipe, left:, right: g.Variable(variable)) -> {
      let left = g.UnlabelledField(left)
      let arguments = [left]
      let function = g.Variable(variable)
      g.Call(function:, arguments:)
    }

    // Nothing to handle.
    _ -> expr
  }
}
