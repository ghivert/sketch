import * as React from 'react'
import * as $gleam from ' ../gleam.mjs'

let context = null

export function context(stylesheet) {
  context ??= React.createContext(stylesheet)
  return context
}

export function get() {
  if (context === null) return $gleam.Result$Error()
  return $gleam.Result$Ok(context)
}
