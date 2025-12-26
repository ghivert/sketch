import * as React from 'react'
import * as $gleam from '../gleam.mjs'

let context_ = null

export function context(stylesheet) {
  context_ ??= React.createContext(stylesheet)
  return context_
}

export function get() {
  if (context_ === null) return $gleam.Result$Error()
  return $gleam.Result$Ok(context_)
}
