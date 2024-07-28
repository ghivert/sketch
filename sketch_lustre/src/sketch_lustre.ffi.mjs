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
  return 'cache' in cache && 'current_id' in cache.cache
}

export function createCssStyleSheet(root) {
  const stylesheet = new CSSStyleSheet()
  if (root && root.adoptedStyleSheets) {
    root.adoptedStyleSheets.push(stylesheet)
  } else {
    document.adoptedStyleSheets.push(stylesheet)
  }
  return stylesheet
}

export function setStylesheet(content, stylesheet) {
  stylesheet.replaceSync(content)
}
