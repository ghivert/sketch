import glance as g
import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import sketch/css
import sketch/css/angle
import sketch/css/keyframe
import sketch/css/length
import sketch/css/media
import sketch/css/transform
import sketch_css/constants
import sketch_css/module/functions
import sketch_css/utils

pub type Value {
  IntValue(Int)
  FloatValue(Float)
  StringValue(String)
  LengthValue(length.Length)
  ClassValue(css.Class)
  AngleValue(angle.Angle)
  TransformValue(transform.Transform)
  StyleValue(css.Style)
  ListValue(List(Value))
  TupleValue(List(Value))
  MediaValue(media.Query)
  AtRuleValue(css.AtRule)
  KeyframeValue(keyframe.Keyframe)
}

pub type Environment =
  List(#(String, Value))

pub type StyleSheet {
  StyleSheet(
    environment: Environment,
    classes: List(#(String, css.Class)),
    at_rules: List(css.AtRule),
    styles: List(#(String, css.Style)),
  )
}

fn init_environment(
  constants: List(g.Definition(g.Constant)),
  modules: List(#(String, StyleSheet)),
) -> Environment {
  use env, constant <- list.fold(constants, [])
  constant.definition.value
  |> convert_expression(StyleSheet(env, [], [], []), modules)
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
    |> list.fold(
      StyleSheet(environment:, classes: [], at_rules: [], styles: []),
      convert_fn,
    )
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
            "keyframes" -> convert_keyframes_call(arguments, env, modules)
            label -> convert_generic_call(label, arguments, env, modules)
          }

        g.FieldAccess(container: g.Variable("sketch/css/svg"), label:) ->
          convert_generic_call(label, arguments, env, modules)

        g.FieldAccess(container: g.Variable("sketch/css/transform"), label:) ->
          convert_transform(label, arguments, env, modules)

        g.FieldAccess(container: g.Variable("sketch/css/media"), label:) ->
          convert_media(label, arguments, env, modules)

        g.FieldAccess(container: g.Variable("sketch/css/length"), label:) ->
          convert_length(label, arguments, env, modules)

        g.FieldAccess(container: g.Variable("sketch/css/angle"), label:) ->
          convert_angle(label, arguments, env, modules)

        g.FieldAccess(container: g.Variable("sketch/css/keyframe"), label:) ->
          convert_keyframe(label, arguments, env, modules)

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
        Ok(AtRuleValue(rule)) -> {
          let at_rules = [rule, ..env.at_rules]
          StyleSheet(..env, at_rules:)
        }
        _ -> env
      }
    }

    _ -> env
  }
}

fn convert_length(
  label: String,
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item:)] -> {
      use value <- result.try(convert_expression(item, env, modules))
      case label, value {
        "px", IntValue(i) -> Ok(LengthValue(length.px(i)))
        "px_", FloatValue(f) -> Ok(LengthValue(length.px_(f)))
        "cm", IntValue(i) -> Ok(LengthValue(length.cm(i)))
        "mm", IntValue(i) -> Ok(LengthValue(length.mm(i)))
        "q", IntValue(i) -> Ok(LengthValue(length.q(i)))
        "in", IntValue(i) -> Ok(LengthValue(length.in(i)))
        "pc", IntValue(i) -> Ok(LengthValue(length.pc(i)))
        "cm_", FloatValue(f) -> Ok(LengthValue(length.cm_(f)))
        "mm_", FloatValue(f) -> Ok(LengthValue(length.mm_(f)))
        "q_", FloatValue(f) -> Ok(LengthValue(length.q_(f)))
        "in_", FloatValue(f) -> Ok(LengthValue(length.in_(f)))
        "pc_", FloatValue(f) -> Ok(LengthValue(length.pc_(f)))
        "pt", IntValue(i) -> Ok(LengthValue(length.pt(i)))
        "pt_", FloatValue(f) -> Ok(LengthValue(length.pt_(f)))
        "percent", IntValue(i) -> Ok(LengthValue(length.percent(i)))
        "percent_", FloatValue(f) -> Ok(LengthValue(length.percent_(f)))
        "vh", IntValue(i) -> Ok(LengthValue(length.vh(i)))
        "vh_", FloatValue(f) -> Ok(LengthValue(length.vh_(f)))
        "vw", IntValue(i) -> Ok(LengthValue(length.vw(i)))
        "vw_", FloatValue(f) -> Ok(LengthValue(length.vw_(f)))
        "em", FloatValue(f) -> Ok(LengthValue(length.em(f)))
        "rem", FloatValue(f) -> Ok(LengthValue(length.rem(f)))
        "lh", FloatValue(f) -> Ok(LengthValue(length.lh(f)))
        "rlh", FloatValue(f) -> Ok(LengthValue(length.rlh(f)))
        "ch", FloatValue(f) -> Ok(LengthValue(length.ch(f)))
        "cap", FloatValue(f) -> Ok(LengthValue(length.cap(f)))
        "ex", FloatValue(f) -> Ok(LengthValue(length.ex(f)))
        "ic", FloatValue(f) -> Ok(LengthValue(length.ic(f)))
        "rcap", FloatValue(f) -> Ok(LengthValue(length.rcap(f)))
        "rch", FloatValue(f) -> Ok(LengthValue(length.rch(f)))
        "rex", FloatValue(f) -> Ok(LengthValue(length.rex(f)))
        "ric", FloatValue(f) -> Ok(LengthValue(length.ric(f)))
        "vmax", IntValue(f) -> Ok(LengthValue(length.vmax(f)))
        "vmax_", FloatValue(f) -> Ok(LengthValue(length.vmax_(f)))
        "vmin", IntValue(f) -> Ok(LengthValue(length.vmin(f)))
        "vmin_", FloatValue(f) -> Ok(LengthValue(length.vmin_(f)))
        "vb", IntValue(f) -> Ok(LengthValue(length.vb(f)))
        "vb_", FloatValue(f) -> Ok(LengthValue(length.vb_(f)))
        "vi", IntValue(f) -> Ok(LengthValue(length.vi(f)))
        "vi_", FloatValue(f) -> Ok(LengthValue(length.vi_(f)))
        "cqw", IntValue(f) -> Ok(LengthValue(length.cqw(f)))
        "cqw_", FloatValue(f) -> Ok(LengthValue(length.cqw_(f)))
        "cqh", IntValue(f) -> Ok(LengthValue(length.cqh(f)))
        "cqh_", FloatValue(f) -> Ok(LengthValue(length.cqh_(f)))
        "cqi", IntValue(f) -> Ok(LengthValue(length.cqi(f)))
        "cqi_", FloatValue(f) -> Ok(LengthValue(length.cqi_(f)))
        "cqb", IntValue(f) -> Ok(LengthValue(length.cqb(f)))
        "cqb_", FloatValue(f) -> Ok(LengthValue(length.cqb_(f)))
        "cqmin", IntValue(f) -> Ok(LengthValue(length.cqmin(f)))
        "cqmin_", FloatValue(f) -> Ok(LengthValue(length.cqmin_(f)))
        "cqmax", IntValue(f) -> Ok(LengthValue(length.cqmax(f)))
        "cqmax_", FloatValue(f) -> Ok(LengthValue(length.cqmax_(f)))
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

fn convert_keyframe(
  label: String,
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item: g.List(items, ..))] -> {
      let values = list.try_map(items, convert_expression(_, env, modules))
      use values <- result.try(values)
      let values =
        list.filter_map(values, fn(value) {
          case value {
            StyleValue(v) -> Ok(v)
            _ -> Error(Nil)
          }
        })
      case label {
        "from" -> Ok(KeyframeValue(keyframe.from(values)))
        "to" -> Ok(KeyframeValue(keyframe.to(values)))
        _ -> Error(Nil)
      }
    }

    [g.UnlabelledField(item:), g.UnlabelledField(item: g.List(items, ..))] -> {
      use item <- result.try(convert_expression(item, env, modules))
      let values = list.try_map(items, convert_expression(_, env, modules))
      use values <- result.try(values)
      case label, item {
        "at", FloatValue(f) ->
          keyframe.at(f, {
            use value <- list.filter_map(values)
            case value {
              StyleValue(v) -> Ok(v)
              _ -> Error(Nil)
            }
          })
          |> KeyframeValue
          |> Ok
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
    [g.UnlabelledField(item: g.Tuple(items))] -> {
      let items = list.try_map(items, convert_expression(_, env, modules))
      use items <- result.try(items)
      case label, items {
        "matrix_3d",
          [
            TupleValue([
              FloatValue(a1),
              FloatValue(b1),
              FloatValue(c1),
              FloatValue(d1),
            ]),
            TupleValue([
              FloatValue(a2),
              FloatValue(b2),
              FloatValue(c2),
              FloatValue(d2),
            ]),
            TupleValue([
              FloatValue(a3),
              FloatValue(b3),
              FloatValue(c3),
              FloatValue(d3),
            ]),
            TupleValue([
              FloatValue(a4),
              FloatValue(b4),
              FloatValue(c4),
              FloatValue(d4),
            ]),
          ]
        -> {
          Ok(
            transform.matrix_3d(
              #(#(a1, b1, c1, d1), #(a2, b2, c2, d2), #(a3, b3, c3, d3), {
                #(a4, b4, c4, d4)
              }),
            ),
          )
        }
        _, _ -> Error(Nil)
      }
    }

    [g.UnlabelledField(item:)] -> {
      use value <- result.try(convert_expression(item, env, modules))
      case label, value {
        "translate_x", LengthValue(f) -> Ok(transform.translate_x(f))
        "translate_y", LengthValue(f) -> Ok(transform.translate_y(f))
        "translate_z", LengthValue(f) -> Ok(transform.translate_z(f))
        "scale_x", FloatValue(f) -> Ok(transform.scale_x(f))
        "scale_y", FloatValue(f) -> Ok(transform.scale_y(f))
        "scale_z", FloatValue(f) -> Ok(transform.scale_z(f))
        "rotate", AngleValue(f) -> Ok(transform.rotate(f))
        "rotate_x", AngleValue(f) -> Ok(transform.rotate_x(f))
        "rotate_y", AngleValue(f) -> Ok(transform.rotate_y(f))
        "rotate_z", AngleValue(f) -> Ok(transform.rotate_z(f))
        "skew_x", AngleValue(f) -> Ok(transform.skew_x(f))
        "skew_y", AngleValue(f) -> Ok(transform.skew_y(f))
        "perspective", LengthValue(f) -> Ok(transform.perspective(f))
        _, _ -> Error(Nil)
      }
    }

    [g.UnlabelledField(item: fst), g.UnlabelledField(item: snd)] -> {
      use fst <- result.try(convert_expression(fst, env, modules))
      use snd <- result.try(convert_expression(snd, env, modules))
      case label, fst, snd {
        "skew", AngleValue(fst), AngleValue(snd) -> Ok(transform.skew(fst, snd))
        "translate", LengthValue(fst), LengthValue(snd) ->
          Ok(transform.translate(fst, snd))
        "scale", FloatValue(fst), FloatValue(snd) ->
          Ok(transform.scale(fst, snd))
        _, _, _ -> Error(Nil)
      }
    }

    [
      g.UnlabelledField(item: fst),
      g.UnlabelledField(item: snd),
      g.UnlabelledField(item: trd),
    ] -> {
      use fst <- result.try(convert_expression(fst, env, modules))
      use snd <- result.try(convert_expression(snd, env, modules))
      use trd <- result.try(convert_expression(trd, env, modules))
      case label, fst, snd, trd {
        "translate_3d", LengthValue(fst), LengthValue(snd), LengthValue(trd) ->
          Ok(transform.translate_3d(fst, snd, trd))
        "scale_3d", FloatValue(fst), FloatValue(snd), FloatValue(trd) ->
          Ok(transform.scale_3d(fst, snd, trd))
        _, _, _, _ -> Error(Nil)
      }
    }

    [
      g.UnlabelledField(item: fst),
      g.UnlabelledField(item: snd),
      g.UnlabelledField(item: trd),
      g.UnlabelledField(item: fth),
    ] -> {
      use fst <- result.try(convert_expression(fst, env, modules))
      use snd <- result.try(convert_expression(snd, env, modules))
      use trd <- result.try(convert_expression(trd, env, modules))
      use fth <- result.try(convert_expression(fth, env, modules))
      case label, fst, snd, trd, fth {
        "rotate_3d",
          FloatValue(fst),
          FloatValue(snd),
          FloatValue(trd),
          AngleValue(fth)
        -> Ok(transform.rotate_3d(fst, snd, trd, fth))
        _, _, _, _, _ -> Error(Nil)
      }
    }

    [
      g.UnlabelledField(item: fst),
      g.UnlabelledField(item: snd),
      g.UnlabelledField(item: trd),
      g.UnlabelledField(item: fth),
      g.UnlabelledField(item: fifth),
      g.UnlabelledField(item: six),
    ] -> {
      use fst <- result.try(convert_expression(fst, env, modules))
      use snd <- result.try(convert_expression(snd, env, modules))
      use trd <- result.try(convert_expression(trd, env, modules))
      use fth <- result.try(convert_expression(fth, env, modules))
      use fifth <- result.try(convert_expression(fifth, env, modules))
      use six <- result.try(convert_expression(six, env, modules))
      case label, fst, snd, trd, fth, fifth, six {
        "matrix",
          FloatValue(fst),
          FloatValue(snd),
          FloatValue(trd),
          FloatValue(fth),
          FloatValue(fifth),
          FloatValue(six)
        -> Ok(transform.matrix(fst, snd, trd, fth, fifth, six))
        _, _, _, _, _, _, _ -> Error(Nil)
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
    "all" -> Ok(media.all())
    "screen" -> Ok(media.screen())
    "print" -> Ok(media.print())
    _ -> convert_media_arguments(label, arguments, env, modules)
  }
  |> result.map(MediaValue)
}

fn convert_media_arguments(label, arguments, env, modules) {
  case arguments {
    [g.UnlabelledField(item:)] -> {
      use value <- result.try(convert_expression(item, env, modules))
      case value, label {
        LengthValue(value), "max_width" -> Ok(media.max_width(value))
        LengthValue(value), "min_width" -> Ok(media.min_width(value))
        LengthValue(value), "max_height" -> Ok(media.max_height(value))
        LengthValue(value), "min_height" -> Ok(media.min_height(value))
        MediaValue(value), "not" -> Ok(media.not(value))
        MediaValue(value), "only" -> Ok(media.only(value))
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

fn convert_keyframes_call(
  arguments: List(g.Field(g.Expression)),
  env: StyleSheet,
  modules: List(#(String, StyleSheet)),
) -> Result(Value, Nil) {
  case arguments {
    [g.UnlabelledField(item:), g.UnlabelledField(item: g.List(elements, ..))] -> {
      use item <- result.try(convert_expression(item, env, modules))
      case item {
        StringValue(s) ->
          css.keyframes(s, {
            use element <- list.flat_map(elements)
            case convert_expression(element, env, modules) {
              Ok(KeyframeValue(v)) -> [v]
              _ -> []
            }
          })
          |> AtRuleValue
          |> Ok
        _ -> Error(Nil)
      }
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
              LengthValue(s) -> length.to_string(s)
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
            LengthValue(s) ->
              StyleValue(css.property(label, length.to_string(s)))
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
