import { cache } from './cache.ffi.mjs'
import * as helpers from './helpers.ffi.mjs'

function determineStyleType(style) {
  if ('class_name' in style && typeof style.class_name === 'string')
    return 'compose'
  if ('key' in style && 'value' in style)
    return 'property'
  if ('pseudo_selector' in style && 'styles' in style)
    return 'pseudoSelector'
  if ('query' in style && 'styles' in style)
    return 'mediaQuery'
}

function wrapClass(id, properties, indent, pseudo = '') {
  const baseIndent = helpers.indent(indent)
  return [`${baseIndent}.${id}${pseudo} {`, ...properties, `${baseIndent}}`].join('\n')
}

function computeProperty(indent, property) {
  const baseIndent = helpers.indent(indent)
  const key = `${baseIndent}${property.key}`
  const important = property.important ? ' !important' : ''
  return `${key}: ${property.value}${important};`
}

function computeProperties(rawProperties, indent = 2) {
  const properties = rawProperties.toArray()
  const init = { properties: [], medias: [], classes: [], pseudoSelectors: [], indent }
  return properties.reduce((accumulator, property) => {
    switch (determineStyleType(property)) {
      case 'compose': {
        const classes = [...accumulator.classes, property.class_name]
        return { ...accumulator, classes }
      }
      case 'property': {
        const cssProperty = computeProperty(accumulator.indent, property)
        const properties = [...accumulator.properties, cssProperty]
        return { ...accumulator, properties }
      }
      case 'pseudoSelector': {
        const { pseudo_selector, styles } = property
        const computedProperties = computeProperties(styles, 4)
        const pseudoSelector = { properties: computedProperties.properties, pseudoSelector: pseudo_selector }
        const pseudoSelectors = [
          ...accumulator.pseudoSelectors,
          ...computedProperties.pseudoSelectors,
          pseudoSelector,
        ]
        return { ...accumulator, pseudoSelectors }
      }
      case 'mediaQuery': {
        const { query, styles } = property
        const { properties, pseudoSelectors } = computeProperties(styles, 4)
        const media = { query, properties, pseudoSelectors }
        const medias = [...accumulator.medias, media]
        return { ...accumulator, medias }
      }
      default:
        return accumulator
    }
  }, init)
}

function computeClasses(id, computedProperties) {
  const { properties, medias, classes, pseudoSelectors } = computedProperties
  const classDef = wrapClass(id, properties, 0)
  const mediasDef = medias.map(({ query, properties, pseudoSelectors }) => {
    const sels = pseudoSelectors.map(p => wrapClass(id, p.properties, 2, p.pseudoSelector))
    return [`${query} {`, wrapClass(id, properties, 2), ...sels, '}'].join('\n')
  })
  const selectorsDef = pseudoSelectors.map(p => wrapClass(id, p.properties, 0, p.pseudoSelector))
  const name = `${classes.join(' ')} ${id}`.trim()
  return { classDef, mediasDef, selectorsDef, name }
}

export function compileClass(args, classId) {
  const className = classId ?? helpers.getFunctionName()
  const content = cache.persist(className)
  if (content) return content
  // Keeping track of test to better display class names.
  // const id = classId?.replace(/[ ,#\.()]/g, '-') ?? uniqueId()
  const id = helpers.uid(className)
  const computedProperties = computeProperties(args)
  const { name, ...definitions } = computeClasses(id, computedProperties)
  cache.store(className, { name, definitions, previousArgs: args, indexRules: null })
  return { name, className }
}

export function memo(klass) {
  cache.memoize(klass)
  return klass
}

export function toString({ name }) {
  return name
}
