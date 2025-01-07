import glance as g
import gleam/list
import gleam/option

pub fn remove(module: g.Module) {
  g.Module(..module, functions: {
    use function <- list.map(module.functions)
    g.Definition(..function, definition: {
      g.Function(..function.definition, body: {
        use statement <- list.map(function.definition.body)
        in_statement(statement)
      })
    })
  })
}

fn in_statement(stat: g.Statement) -> g.Statement {
  case stat {
    g.Use(..) -> g.Use(..stat, function: in_expr(stat.function))
    g.Expression(e) -> g.Expression(in_expr(e))
    g.Assignment(..) -> g.Assignment(..stat, value: in_expr(stat.value))
  }
}

fn in_expr(expr: g.Expression) -> g.Expression {
  case expr {
    // Deeply rewrites.
    g.NegateInt(expr) -> g.NegateInt(in_expr(expr))
    g.NegateBool(expr) -> g.NegateBool(in_expr(expr))
    g.Block(stat) -> g.Block(list.map(stat, in_statement))
    g.Todo(stat) -> g.Todo(option.map(stat, in_expr))
    g.Panic(stat) -> g.Panic(option.map(stat, in_expr))
    g.Tuple(stat) -> g.Tuple(list.map(stat, in_expr))
    g.Fn(..) -> g.Fn(..expr, body: list.map(expr.body, in_statement))
    g.TupleIndex(..) -> g.TupleIndex(..expr, tuple: in_expr(expr.tuple))

    g.List(elements, rest) -> {
      let elements = list.map(elements, in_expr)
      let rest = option.map(rest, in_expr)
      g.List(elements:, rest:)
    }

    g.RecordUpdate(..) -> {
      let record = in_expr(expr.record)
      g.RecordUpdate(..expr, record:, fields: {
        use field <- list.map(expr.fields)
        let item = option.map(field.item, in_expr)
        g.RecordUpdateField(..field, item:)
      })
    }

    g.FieldAccess(..) -> {
      let container = in_expr(expr.container)
      g.FieldAccess(..expr, container:)
    }

    g.Call(..) -> {
      let function = in_expr(expr.function)
      g.Call(function:, arguments: {
        use argument <- list.map(expr.arguments)
        case argument {
          g.UnlabelledField(e) -> g.UnlabelledField(in_expr(e))
          g.ShorthandField(label:) -> g.ShorthandField(label:)
          g.LabelledField(label:, item:) ->
            g.LabelledField(label:, item: in_expr(item))
        }
      })
    }

    g.Case(..) -> {
      let subjects = list.map(expr.subjects, in_expr)
      g.Case(subjects:, clauses: {
        use clause <- list.map(expr.clauses)
        let guard = option.map(clause.guard, in_expr)
        let body = in_expr(clause.body)
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
