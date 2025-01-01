export function wrap(current) {
  if (isPersistent(current)) return { current }
  return current
}

export function set(variable, newValue) {
  if (!('current' in variable)) return newValue
  variable.current = newValue
  return variable
}

export function get(variable) {
  if ('current' in variable) return variable.current
  return variable
}

export function isPersistent(cache) {
  return 'cache' in cache && 'is_persistent' in cache && cache.is_persistent
}
