export function wrap(current) {
  return { current }
}

export function set(variable, newValue) {
  variable.current = newValue
  return variable
}

export function get(variable) {
  return variable.current
}
