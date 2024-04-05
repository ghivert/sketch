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

// Compare two data structures to check if they're the same.
export function deepEqual(a, b) {
  const consts = ['string', 'number', 'boolean']
  if (consts.includes(typeof a) || consts.includes(typeof b)) return a === b
  for (const value in a) {
    if (!(value in b)) return false
    const isSame = deepEqual(a[value], b[value])
    if (!isSame) return false
  }
  return true
}

// A Style property can be of four types: a class composition, a property definition
// a pseudo-selector definitions or a media query definition.
// They're defined in Gleam, and the class is opaque, so the only way is to
// read in the content of the object to check the interesting fields.
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

// Take a class definition, and turns it to something like
// .className {
//   property1: value;
//   property2: value;
// }
export function wrapClass(id, properties, indent_, pseudo = '') {
  const baseIndent = indent(indent_)
  return [`${baseIndent}.${id}${pseudo} {`, ...properties, `${baseIndent}}`].join('\n')
}

// Turn the different components of a property into the correct CSS property.
// I.e. property: value [!important];
export function computeProperty(indent_, property) {
  const baseIndent = indent(indent_)
  const key = `${baseIndent}${property.key}`
  const important = property.important ? ' !important' : ''
  return `${key}: ${property.value}${important};`
}
