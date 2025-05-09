import * as gleam from '../../../../gleam.mjs'

let currentStylesheet = null
const stylesheets = {}

export function setStyleSheet(stylesheet) {
  stylesheets[stylesheet.id] = stylesheet
  return new gleam.Ok(stylesheet)
}

export function setCurrentStylesheet(stylesheet) {
  currentStylesheet = stylesheet.id
  return new gleam.Ok(stylesheet)
}

export function getStyleSheet() {
  const stylesheet = stylesheets[currentStylesheet]
  if (!stylesheet) return new gleam.Error()
  return new gleam.Ok(stylesheet)
}
