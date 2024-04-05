export function indent(size) {
  const space = ' '
  return space.repeat(size)
}

export const uid = (function() {
  let id = 0
  const classNames = {}
  return function(className) {
    classNames[className] ??= (id++).toString().padStart(4, '0')
    const index = classNames[className]
    return `css-${index}`
  }
})()

export function getFunctionName() {
  const error = new Error()
  if (!error.stack) throw new Error('Unable to find the stacktrace and to infer the className')
  const stack = error.stack ?? ''
  return stack.split('\n').slice(1, 5).join('\n')
}

export function deepEqual(args, previousArgs) {
  const constants = ['string', 'number', 'boolean']
  if (constants.includes(typeof args) || constants.includes(typeof previousArgs)) return args === previousArgs
  for (const value in args) {
    if (!(value in previousArgs)) return false
    const isSame = deepEqual(args[value], previousArgs[value])
    if (!isSame) return false
  }
  return true
}

export function determineStyleType(style) {
  if ('class_name' in style && typeof style.class_name === 'string') {
    return 'compose'
  } else if ('key' in style && 'value' in style) {
    return 'property'
  } else if ('pseudo_selector' in style && 'styles' in style) {
    return 'pseudoSelector'
  } else if ('query' in style && 'styles' in style) {
    return 'mediaQuery'
  }
}

export function wrapClass(id, properties, indent_, pseudo = '') {
  const baseIndent = indent(indent_)
  return [`${baseIndent}.${id}${pseudo} {`, ...properties, `${baseIndent}}`].join('\n')
}

export function computeProperty(indent_, property) {
  const baseIndent = indent(indent_)
  const key = `${baseIndent}${property.key}`
  const important = property.important ? ' !important' : ''
  return `${key}: ${property.value}${important};`
}
