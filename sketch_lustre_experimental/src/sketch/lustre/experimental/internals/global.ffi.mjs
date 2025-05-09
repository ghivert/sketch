import * as gleam from '../../../../gleam.mjs'

let _stylesheet = null

export function setStyleSheet(stylesheet) {
  _stylesheet = stylesheet
  return new gleam.Ok(stylesheet)
}

export function getStyleSheet() {
  if (!_stylesheet) return new gleam.Error()
  return new gleam.Ok(_stylesheet)
}
