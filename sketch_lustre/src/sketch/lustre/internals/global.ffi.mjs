import * as gleam from '../../../gleam.mjs'

let currentStylesheet = null
const stylesheets = {}

export function setStyleSheet(stylesheet) {
  stylesheets[stylesheet.id] = stylesheet
  return gleam.Result$Ok(stylesheet)
}

export function teardownStyleSheet(stylesheet) {
  delete stylesheets[stylesheet.id]
  return gleam.Result$Ok(undefined)
}

export function setCurrentStylesheet(stylesheet) {
  currentStylesheet = stylesheet.id
  return gleam.Result$Ok(stylesheet)
}

export function getStyleSheet() {
  const stylesheet = stylesheets[currentStylesheet]
  if (!stylesheet) return gleam.Result$Error()
  return gleam.Result$Ok(stylesheet)
}

export function dismissCurrentStylesheet() {
  currentStylesheet = null
  return gleam.Result$Ok(undefined)
}
