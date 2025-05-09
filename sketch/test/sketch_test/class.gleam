import birdie
import classes/globals_css
import gleam/string
import sketch
import sketch_test/helpers
import startest.{describe, it}
import startest/expect

import classes/dimensions_css
import classes/edges_css
import classes/font_face_css
import classes/important_css
import classes/keyframe_css
import classes/medias_css
import classes/nestings_css

pub fn sketch_tests() {
  describe("Sketch", [
    describe("Class generation", [
      it("should handle dimensions", fn() {
        dimensions_css.dimensions_variables()
        |> helpers.compute_class("dimensions_css")
      }),
      it("should handle edges cases", fn() {
        edges_css.edge_cases("blue")
        |> helpers.compute_class("edges_css")
      }),
      it("should handle font-face rules", fn() {
        font_face_css.font_face()
        |> helpers.compute_at_rule("font_face_css")
      }),
      it("should handle important properties", fn() {
        important_css.important()
        |> helpers.compute_class("important_css")
      }),
      it("should handle keyframe rules", run_keyframes),
      it("should handle media queries", run_medias),
      it("should handle nestings properties", run_nestings),
      it("should handle global classes", run_globals),
    ]),
  ])
}

fn run_keyframes() {
  keyframe_css.keyframe() |> helpers.compute_at_rule("keyframe_css")
  let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Ephemeral)
  let stylesheet = sketch.at_rule(keyframe_css.keyframe(), stylesheet)
  let #(stylesheet, _) = sketch.class_name(keyframe_css.example(), stylesheet)
  let content = sketch.render(stylesheet)
  birdie.snap(title: helpers.multitarget_title("keyframe_class"), content:)
  Nil
}

fn run_medias() {
  medias_css.and() |> helpers.compute_class("medias_and")
  medias_css.and_or() |> helpers.compute_class("medias_and_or")
  medias_css.or() |> helpers.compute_class("medias_or")
  medias_css.pseudo_class() |> helpers.compute_class("medias_pseudo_class")
  medias_css.simple() |> helpers.compute_class("medias_simple")
}

fn run_nestings() {
  nestings_css.content("blue") |> helpers.compute_class("nestings_css")
  nestings_css.example("blue") |> helpers.compute_class("nestings_example")
}

fn run_globals() {
  let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Ephemeral)
  let stylesheet = sketch.global(stylesheet, globals_css.root("red"))
  let content = sketch.render(stylesheet)
  content |> string.contains(":root") |> expect.to_be_true
  birdie.snap(title: "run_globals_red", content:)

  let stylesheet = sketch.global(stylesheet, globals_css.root("blue"))
  let content = sketch.render(stylesheet)
  content |> string.contains(":root") |> expect.to_be_true
  birdie.snap(title: "run_globals_blue", content:)

  Nil
}
