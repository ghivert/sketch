const cache = {}

const idt = indent => ' '.repeat(indent)
let id_ = 0
const uniqueId = () => `css-${(id_++).toString().padStart(4, '0')}`

const styleSheet = (() => {
  const styleElement = document.createElement('style')
  document.head.appendChild(styleElement)
  if (!styleElement.sheet) throw new Error('StyleSheet not found, styled cannot be used.')
  return styleElement.sheet
})()

function getCallingFunction() {
  const error = new Error()
  if (!error.stack) throw new Error('Unable to find the stacktrace and to infer the className')
  const stack = error.stack ?? ''
  return stack.split('\n').slice(1, 5).join('\n')
}

function computeProperties(rawProperties, indent = 2) {
  const properties = rawProperties.toArray()
  const init = { properties: [], medias: [], classes: [], pseudoSelectors: [], indent }
  return properties.reduce((acc, property) => {
    const { properties, medias, classes, pseudoSelectors, indent } = acc
    const baseIndent = idt(indent)
    if ('class_name' in property && typeof property.class_name === 'string') {
      return { properties, medias, classes: [...classes, property.class_name], pseudoSelectors, indent }
    }
    if ('key' in property && 'value' in property) {
      const cssProperty = `${baseIndent}${property.key}: ${property.value}${property.important ? ' !important' : ''};`
      return { properties: [...properties, cssProperty], medias, classes, pseudoSelectors, indent }
    }
    if ('pseudo_selector' in property && 'styles' in property) {
      const { pseudo_selector, styles } = property
      const computedProperties = computeProperties(styles, 4)
      return {
        properties,
        medias,
        classes,
        pseudoSelectors: [
          ...pseudoSelectors,
          ...computedProperties.pseudoSelectors,
          {
            properties: computedProperties.properties,
            pseudoSelector: pseudo_selector,
          },
        ],
      }
    }
    if ('query' in property && 'styles' in property) {
      const { query, styles } = property
      const computedProperties = computeProperties(styles, 4)
      return {
        properties,
        medias: [
          ...medias,
          {
            query,
            properties: computedProperties.properties,
            pseudoSelectors: computedProperties.pseudoSelectors,
          },
        ],
        pseudoSelectors,
        classes,
        indent,
      }
    }
    return { properties, medias, classes, pseudoSelectors, indent }
  }, init)
}

const sameObjects = (args, previousArgs) => {
  if (
    typeof args === 'string' ||
    typeof previousArgs === 'string' ||
    typeof args === 'number' ||
    typeof previousArgs === 'number' ||
    typeof args === 'boolean' ||
    typeof previousArgs === 'boolean'
  )
    return args === previousArgs
  if (typeof args !== 'object' || typeof previousArgs !== 'object') return false
  for (const value in args) {
    if (!(value in previousArgs)) return false
    const isSame = sameObjects(args[value], previousArgs[value])
    if (!isSame) return false
  }
  return true
}

export function compileClass(args, classId) {
  const callingClass = classId ?? getCallingFunction()
  const content = cache[callingClass]
  if (content) {
    if (content.memoized) return { name: content.name, callingClass }
    const isSame = sameObjects(args, content.previousArgs)
    if (isSame) return { name: content.name, callingClass }
    if (!isSame) styleSheet.deleteRule(content.indexRule)
  }

  // Keeping track of test to better display class names.
  // const id = classId?.replace(/[ ,#\.()]/g, '-') ?? uniqueId()
  const id = uniqueId()
  const { properties, medias, classes, pseudoSelectors } = computeProperties(args)
  const wrapClass = (properties, indent, pseudo = '') => {
    const baseIndent = idt(indent)
    return [`${baseIndent}.${id}${pseudo} {`, ...properties, `${baseIndent}}`].join('\n')
  }
  const classDef = wrapClass(properties, 0)
  const mediasDef = medias.map(({ query, properties, pseudoSelectors }) => {
    const rule = [`${query} {`, wrapClass(properties, 2)].join('\n')
    const sels = pseudoSelectors.map(({ properties, pseudoSelector }) => wrapClass(properties, 2, pseudoSelector))
    return [rule, ...sels, '}'].join('\n')
  })
  const selectors = pseudoSelectors.map(({ properties, pseudoSelector }) => wrapClass(properties, 0, pseudoSelector))
  mediasDef.forEach(def => styleSheet.insertRule(def))
  selectors.forEach(def => styleSheet.insertRule(def))
  const indexRule = styleSheet.insertRule(classDef)
  const name = `${classes.join(' ')} ${id}`.trim()
  cache[callingClass] = { name, previousArgs: args, indexRule, memoized: false }
  return { name, callingClass }
}

export function memo(klass) {
  cache[klass.callingClass].memoized = true
  return klass
}

export function toString({ name }) {
  return name
}
