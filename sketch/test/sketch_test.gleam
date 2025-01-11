import birdie
import gleeunit
import sketch
import test_/helpers

import classes/aliased_css
import classes/dimensions_css
import classes/edges_css
import classes/exposed_css
import classes/font_face_css
import classes/function_css
import classes/important_css
import classes/keyframe_css
import classes/medias_css
import classes/nestings_css
import classes/variable_css

pub fn main() {
  gleeunit.main()
}

pub fn aliased_test() {
  aliased_css.aliased_module()
  |> helpers.compute_class("aliased_css")
}

pub fn dimensions_test() {
  dimensions_css.dimensions_variables()
  |> helpers.compute_class("dimensions_css")
}

pub fn edges_test() {
  edges_css.edge_cases("blue")
  |> helpers.compute_class("edges_css")
}

pub fn exposed_test() {
  exposed_css.exposed_class()
  |> helpers.compute_class("exposed_css")
  exposed_css.exposed_property()
  |> helpers.compute_class("exposed_property")
}

pub fn function_test() {
  function_css.class()
  |> helpers.compute_class("function_css")
  function_css.property()
  |> helpers.compute_class("function_property")
}

pub fn important_test() {
  important_css.important()
  |> helpers.compute_class("important_css")
}

pub fn medias_test() {
  medias_css.and()
  |> helpers.compute_class("medias_and")
  medias_css.and_or()
  |> helpers.compute_class("medias_and_or")
  medias_css.or()
  |> helpers.compute_class("medias_or")
  medias_css.pseudo_class()
  |> helpers.compute_class("medias_pseudo_class")
  medias_css.simple()
  |> helpers.compute_class("medias_simple")
}

pub fn nestings_test() {
  nestings_css.content("blue")
  |> helpers.compute_class("nestings_css")
  nestings_css.example("blue")
  |> helpers.compute_class("nestings_example")
}

pub fn variable_test() {
  variable_css.variable_property()
  |> helpers.compute_class("variable_css")
}

pub fn keyframe_test() {
  keyframe_css.keyframe()
  |> helpers.compute_at_rule("keyframe_css")

  let assert Ok(stylesheet) = sketch.stylesheet(strategy: sketch.Ephemeral)
  let stylesheet = sketch.at_rule(keyframe_css.keyframe(), stylesheet)
  let #(stylesheet, _) = sketch.class_name(keyframe_css.example(), stylesheet)
  let content = sketch.render(stylesheet)
  birdie.snap(title: helpers.multitarget_title("keyframe_class"), content:)
}

pub fn font_face_test() {
  font_face_css.font_face()
  |> helpers.compute_at_rule("font_face_css")
}
