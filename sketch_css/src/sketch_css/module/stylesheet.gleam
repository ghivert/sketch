import glance as g
import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import sketch/css
import sketch/css/angle
import sketch/css/media
import sketch/css/size
import sketch/css/transform
import sketch_css/constants
import sketch_css/module/functions
import sketch_css/utils

pub type Value {
  IntValue(Int)
  FloatValue(Float)
  StringValue(String)
  SizeValue(size.Size)
  ClassValue(css.Class)
  AngleValue(angle.Angle)
  TransformValue(transform.Transform)
  StyleValue(css.Style)
  ListValue(List(Value))
  TupleValue(List(Value))
  MediaValue(media.Query)
}

pub type Environment =
  List(#(String, Value))

pub type StyleSheet {
  StyleSheet(
    environment: Environment,
    classes: List(#(String, css.Class)),
    styles: List(#(String, css.Style)),
  )
}

fn init_environment(
  constants: List(g.Definition(g.Constant)),
  modules: List(#(String, StyleSheet)),
) -> Environment {
  use env, constant <- list.fold(constants, [])
  constant.definition.value
  |> convert_expression(StyleSheet(env, [], []), modules)
  |> result.map(list.key_set(env, constant.definition.name, _))
  |> result.unwrap(env)
}

pub fn convert(
  modules: List(#(String, StyleSheet)),
  module: #(String, g.Module),
) -> List(#(String, StyleSheet)) {
  let #(name, g.Module(constants:, functions:, ..)) = module
  let environment = init_environment(constants, modules)
  let convert_fn = convert_functions(modules)
  let stylesheet =
    functions
    |> list.map(fn(function) { function.definition })
    |> list.sort(functions.is_dependent)
    |> list.fold(StyleSheet(environment:, classes: [], styles: []), convert_fn)
  [#(name, StyleSheet(..stylesheet, environment:)), ..modules]
}

fn convert_expression(
  value: g.Expression,
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case value {
    g.Int(int) -> int.parse(int) |> result.map(IntValue)
    g.Float(float) -> float.parse(float) |> result.map(FloatValue)
    g.String(string) -> Ok(StringValue(string))
    g.Variable(variable) -> list.key_find(env.environment, variable)

    g.List(elements:, ..) -> {
      elements
      |> list.try_map(convert_expression(_, env, modules))
      |> result.map(ListValue)
    }

    g.Tuple(expressions) -> {
      expressions
      |> list.try_map(convert_expression(_, env, modules))
      |> result.map(TupleValue)
    }

    g.TupleIndex(tuple:, index:) -> {
      use value <- result.try(convert_expression(tuple, env, modules))
      case value {
        TupleValue(elements) -> utils.at(elements, index)
        _ -> Error(Nil)
      }
    }

    g.FieldAccess(container: g.Variable(module), label:) -> {
      use module <- result.try(list.key_find(modules, module))
      module.environment
      |> list.key_find(label)
      |> result.try_recover(fn(_) {
        module.styles
        |> list.key_find(label)
        |> result.map(StyleValue)
      })
      |> result.try_recover(fn(_) {
        module.classes
        |> list.key_find(label)
        |> result.map(ClassValue)
      })
    }

    g.Call(function:, arguments:) -> {
      case function {
        g.FieldAccess(container: g.Variable("sketch/css"), label:) ->
          case label {
            "class" -> convert_class_call(arguments, env, modules)
            "compose" -> convert_compose_call(arguments, env, modules)
            "media" -> convert_media_call(arguments, env, modules)
            "selector" -> convert_selector_call(arguments, env, modules)
            "property" -> convert_property_call(arguments, env, modules)
            "important" -> convert_important_call(arguments, env, modules)
            "transform" -> convert_transform_call(arguments, env, modules)
            label -> convert_generic_call(label, arguments, env, modules)
          }

        g.FieldAccess(container: g.Variable("sketch/css/svg"), label:) ->
          convert_generic_call(label, arguments, env, modules)

        g.FieldAccess(container: g.Variable("sketch/css/transform"), label:) ->
          convert_transform(label, arguments, env, modules)

        g.FieldAccess(container: g.Variable("sketch/css/media"), label:) ->
          convert_media(label, arguments, env, modules)

        g.FieldAccess(container: g.Variable("sketch/css/size"), label:) ->
          convert_size(label, arguments, env, modules)

        g.FieldAccess(container: g.Variable("sketch/css/angle"), label:) ->
          convert_angle(label, arguments, env, modules)

        g.FieldAccess(container: g.Variable(container), label:) -> {
          let stylesheet = list.key_find(modules, container)
          let classes = result.map(stylesheet, fn(s) { s.classes })
          let styles = result.map(stylesheet, fn(s) { s.styles })
          classes
          |> result.then(list.key_find(_, label))
          |> result.map(ClassValue)
          |> result.try_recover(fn(_) {
            styles
            |> result.then(list.key_find(_, label))
            |> result.map(StyleValue)
          })
        }

        g.Variable(label) -> {
          env.classes
          |> list.key_find(label)
          |> result.map(ClassValue)
          |> result.try_recover(fn(_) {
            env.styles
            |> list.key_find(label)
            |> result.map(StyleValue)
          })
        }

        _ -> Error(Nil)
      }
    }

    _ -> Error(Nil)
  }
}

fn convert_functions(modules: List(#(String, StyleSheet))) {
  fn(env: StyleSheet, function: g.Function) -> StyleSheet {
    use <- bool.guard(when: function.publicity == g.Private, return: env)
    let env = add_function_parameters(function, env)
    convert_body(function, env, modules)
  }
}

fn add_function_parameters(function: g.Function, env: StyleSheet) {
  use env, parameter <- list.fold(function.parameters, env)
  case parameter.name {
    g.Discarded(_) -> env
    g.Named(argument) -> {
      let var = "var(--" <> argument <> ")"
      let var = StringValue(var)
      let environment = [#(argument, var), ..env.environment]
      StyleSheet(..env, environment:)
    }
  }
}

fn convert_body(
  function: g.Function,
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> StyleSheet {
  use env, statement <- list.fold(function.body, env)
  case statement {
    g.Assignment(pattern: g.PatternVariable(name:), value:, ..) -> {
      case convert_expression(value, env, modules) {
        Error(_) -> env
        Ok(value) -> {
          list.key_set(env.environment, name, value)
          |> fn(e) { StyleSheet(..env, environment: e) }
        }
      }
    }

    g.Expression(expression) -> {
      case convert_expression(expression, env, modules) {
        Ok(ClassValue(class)) -> {
          let classes = [#(function.name, class), ..env.classes]
          StyleSheet(..env, classes:)
        }
        Ok(StyleValue(style)) -> {
          let styles = [#(function.name, style), ..env.styles]
          StyleSheet(..env, styles:)
        }
        _ -> env
      }
    }

    _ -> env
  }
}

fn convert_size(
  label: String,
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item:)] -> {
      use value <- result.try(convert_expression(item, env, modules))
      case label, value {
        "px", IntValue(i) -> Ok(SizeValue(size.px(i)))
        "px_", FloatValue(f) -> Ok(SizeValue(size.px_(f)))
        "pt", IntValue(i) -> Ok(SizeValue(size.pt(i)))
        "pt_", FloatValue(f) -> Ok(SizeValue(size.pt_(f)))
        "percent", IntValue(i) -> Ok(SizeValue(size.percent(i)))
        "percent_", FloatValue(f) -> Ok(SizeValue(size.percent_(f)))
        "vh", IntValue(i) -> Ok(SizeValue(size.vh(i)))
        "vh_", FloatValue(f) -> Ok(SizeValue(size.vh_(f)))
        "vw", IntValue(i) -> Ok(SizeValue(size.vw(i)))
        "vw_", FloatValue(f) -> Ok(SizeValue(size.vw_(f)))
        "em", FloatValue(f) -> Ok(SizeValue(size.em(f)))
        "rem", FloatValue(f) -> Ok(SizeValue(size.rem(f)))
        "lh", FloatValue(f) -> Ok(SizeValue(size.lh(f)))
        "rlh", FloatValue(f) -> Ok(SizeValue(size.rlh(f)))
        "ch", IntValue(i) -> Ok(SizeValue(size.ch(i)))
        "ch_", FloatValue(f) -> Ok(SizeValue(size.ch_(f)))
        _, _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn convert_angle(
  label: String,
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item:)] -> {
      use value <- result.try(convert_expression(item, env, modules))
      case label, value {
        "deg", FloatValue(f) -> Ok(AngleValue(angle.deg(f)))
        "rad", FloatValue(f) -> Ok(AngleValue(angle.rad(f)))
        "grad", FloatValue(f) -> Ok(AngleValue(angle.grad(f)))
        "turn", FloatValue(f) -> Ok(AngleValue(angle.turn(f)))
        _, _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn convert_transform(
  label: String,
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item:)] -> {
      use value <- result.try(convert_expression(item, env, modules))
      case label, value {
        "translate", SizeValue(f) -> Ok(transform.translate(f))
        "translate_x", SizeValue(f) -> Ok(transform.translate_x(f))
        "translate_y", SizeValue(f) -> Ok(transform.translate_y(f))
        "scale", FloatValue(f) -> Ok(transform.scale(f))
        "scale_x", FloatValue(f) -> Ok(transform.scale_x(f))
        "scale_y", FloatValue(f) -> Ok(transform.scale_y(f))
        "rotate", AngleValue(f) -> Ok(transform.rotate(f))
        "skew_x", AngleValue(f) -> Ok(transform.skew_x(f))
        "skew_y", AngleValue(f) -> Ok(transform.skew_y(f))
        _, _ -> Error(Nil)
      }
    }

    [g.UnlabelledField(item: fst), g.UnlabelledField(item: snd)] -> {
      use fst <- result.try(convert_expression(fst, env, modules))
      use snd <- result.try(convert_expression(snd, env, modules))
      case label, fst, snd {
        "translate2", SizeValue(fst), SizeValue(snd) ->
          Ok(transform.translate2(fst, snd))
        "scale2", FloatValue(fst), FloatValue(snd) ->
          Ok(transform.scale2(fst, snd))
        _, _, _ -> Error(Nil)
      }
    }

    _ -> Error(Nil)
  }
  |> result.map(TransformValue)
}

fn convert_media(
  label: String,
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case label {
    "dark_theme" -> Ok(media.dark_theme())
    "light_theme" -> Ok(media.light_theme())
    "landscape" -> Ok(media.landscape())
    "portrait" -> Ok(media.portrait())
    _ -> convert_media_arguments(label, arguments, env, modules)
  }
  |> result.map(MediaValue)
}

fn convert_media_arguments(label, arguments, env, modules) {
  case arguments {
    [g.UnlabelledField(item:)] -> {
      use value <- result.try(convert_expression(item, env, modules))
      case value, label {
        SizeValue(value), "max_width" -> Ok(media.max_width(value))
        SizeValue(value), "min_width" -> Ok(media.min_width(value))
        SizeValue(value), "max_height" -> Ok(media.max_height(value))
        SizeValue(value), "min_height" -> Ok(media.min_height(value))
        MediaValue(value), "not" -> Ok(media.not(value))
        _, _ -> Error(Nil)
      }
    }

    [g.UnlabelledField(item: fst), g.UnlabelledField(item: snd)] -> {
      use fst <- result.try(convert_expression(fst, env, modules))
      use snd <- result.try(convert_expression(snd, env, modules))
      case fst, snd, label {
        MediaValue(fst), MediaValue(snd), "and" -> Ok(media.and(fst, snd))
        MediaValue(fst), MediaValue(snd), "or" -> Ok(media.or(fst, snd))
        _, _, _ -> Error(Nil)
      }
    }

    _ -> Error(Nil)
  }
}

fn convert_class_call(
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item: g.List(elements, ..))] -> {
      css.class({
        use element <- list.map(elements)
        case convert_expression(element, env, modules) {
          Ok(StyleValue(v)) -> v
          _ -> css.none()
        }
      })
      |> ClassValue
      |> Ok
    }
    _ -> Error(Nil)
  }
}

fn convert_compose_call(
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item:)] -> {
      use value <- result.map(convert_expression(item, env, modules))
      case value {
        ClassValue(class) -> StyleValue(css.compose(class))
        others -> others
      }
    }
    _ -> Error(Nil)
  }
}

fn convert_important_call(
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item:)] -> {
      use value <- result.map(convert_expression(item, env, modules))
      case value {
        StyleValue(style) -> StyleValue(css.important(style))
        others -> others
      }
    }
    _ -> Error(Nil)
  }
}

fn convert_media_call(
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item:), g.UnlabelledField(item: g.List(styles, ..))] -> {
      use value <- result.map(convert_expression(item, env, modules))
      case value {
        MediaValue(query) ->
          StyleValue({
            css.media(query, {
              use style <- list.filter_map(styles)
              use value <- result.map(convert_expression(style, env, modules))
              case value {
                StyleValue(s) -> s
                _ -> css.none()
              }
            })
          })
        others -> others
      }
    }
    _ -> Error(Nil)
  }
}

fn convert_selector_call(
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item:), g.UnlabelledField(item: g.List(styles, ..))] -> {
      use selector <- result.map(convert_expression(item, env, modules))
      case selector {
        StringValue(prop) ->
          css.selector(prop, {
            use style <- list.filter_map(styles)
            use value <- result.map(convert_expression(style, env, modules))
            case value {
              StyleValue(s) -> s
              _ -> css.none()
            }
          })
        _ -> css.none()
      }
      |> StyleValue
    }
    _ -> Error(Nil)
  }
}

fn convert_property_call(
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item: property), g.UnlabelledField(item: value)] -> {
      use property <- result.try(convert_expression(property, env, modules))
      use value <- result.map(convert_expression(value, env, modules))
      case property, value {
        StringValue(prop), StringValue(value) -> css.property(prop, value)
        _, _ -> css.none()
      }
      |> StyleValue
    }
    _ -> Error(Nil)
  }
}

fn convert_transform_call(
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item: g.List(transforms, ..))] -> {
      let styles = list.try_map(transforms, convert_expression(_, env, modules))
      use styles <- result.try(styles)
      use transforms <- result.map({
        use value <- list.try_map(styles)
        case value {
          TransformValue(v) -> Ok(v)
          _ -> Error(Nil)
        }
      })
      StyleValue(css.transform(transforms))
    }
    _ -> Error(Nil)
  }
}

fn convert_generic_call(
  label: String,
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  let is_pseudo = list.key_find(constants.pseudo_classes, label)
  let is_combinator = list.key_find(constants.combinators, label)
  case is_pseudo, is_combinator {
    Ok(c), _ -> convert_pseudo_selector_call(label, c, arguments, env, modules)
    _, Ok(c) -> convert_combinator_call(c, arguments, env, modules)
    _, _ -> {
      let label = string.replace(label, each: "_", with: "-")
      case arguments {
        [g.UnlabelledField(item: g.List(styles, ..))] -> {
          let styles = list.try_map(styles, convert_expression(_, env, modules))
          use styles <- result.map(styles)
          list.map(styles, fn(value) {
            case value {
              StringValue(s) -> s
              IntValue(i) -> int.to_string(i)
              FloatValue(f) -> float.to_string(f)
              SizeValue(s) -> size.to_string(s)
              AngleValue(a) -> angle.to_string(a)
              _ -> ""
            }
          })
          |> list.map(fn(s) { "\"" <> s <> "\"" })
          |> string.join(with: " ")
          |> css.property(label, _)
          |> StyleValue
        }
        [g.UnlabelledField(item:)] -> {
          use value <- result.map(convert_expression(item, env, modules))
          case value {
            StringValue(s) -> StyleValue(css.property(label, s))
            IntValue(i) -> StyleValue(css.property(label, int.to_string(i)))
            FloatValue(f) -> StyleValue(css.property(label, float.to_string(f)))
            SizeValue(s) -> StyleValue(css.property(label, size.to_string(s)))
            AngleValue(a) -> StyleValue(css.property(label, angle.to_string(a)))
            _ -> StyleValue(css.none())
          }
        }
        _ -> Error(Nil)
      }
    }
  }
}

fn convert_pseudo_selector_call(
  label: String,
  class: String,
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case label {
    "state"
    | "dir"
    | "nth_child"
    | "nth_last_child"
    | "nth_of_type"
    | "nth_last_of_type" -> {
      case arguments {
        [g.UnlabelledField(item:), g.UnlabelledField(g.List(styles, ..))] -> {
          use selector <- result.map(convert_expression(item, env, modules))
          case selector {
            StringValue(prop) ->
              css.selector(class <> "(" <> prop <> ")", {
                use style <- list.filter_map(styles)
                use value <- result.map(convert_expression(style, env, modules))
                case value {
                  StyleValue(s) -> s
                  _ -> css.none()
                }
              })
            _ -> css.none()
          }
          |> StyleValue
        }
        _ -> Error(Nil)
      }
    }
    _ -> {
      case arguments {
        [g.UnlabelledField(g.List(styles, ..))] -> {
          css.selector(class, {
            use style <- list.filter_map(styles)
            use value <- result.map(convert_expression(style, env, modules))
            case value {
              StyleValue(s) -> s
              _ -> css.none()
            }
          })
          |> StyleValue
          |> Ok
        }
        _ -> Error(Nil)
      }
    }
  }
}

fn convert_combinator_call(
  combinator: fn(css.Class, List(css.Style)) -> css.Style,
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item:), g.UnlabelledField(g.List(styles, ..))] -> {
      use class <- result.map(convert_expression(item, env, modules))
      case class {
        ClassValue(class) ->
          combinator(class, {
            use style <- list.filter_map(styles)
            use value <- result.map(convert_expression(style, env, modules))
            case value {
              StyleValue(s) -> s
              _ -> css.none()
            }
          })
        _ -> css.none()
      }
      |> StyleValue
    }
    _ -> Error(Nil)
  }
}
