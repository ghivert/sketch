import * as gleam from './gleam.mjs'
import { cache } from './cache.ffi.mjs'
import * as helpers from './helpers.ffi.mjs'

function computeProperties(rawProperties, indent = 2) {
  const properties = rawProperties.toArray()
  const init = { properties: [], medias: [], classes: [], pseudoSelectors: [], indent }
  return properties.reduce((acc, property) => {
    const { properties, medias, classes, pseudoSelectors, indent } = acc
    const baseIndent = helpers.indent(indent)
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

export function compileClass(args, classId) {
  const className = classId ?? helpers.getFunctionName()
  const content = cache.persist(className)
  if (content) return content

  // Keeping track of test to better display class names.
  // const id = classId?.replace(/[ ,#\.()]/g, '-') ?? uniqueId()
  const id = helpers.uid()
  const { properties, medias, classes, pseudoSelectors } = computeProperties(args)
  const wrapClass = (properties, indent, pseudo = '') => {
    const baseIndent = helpers.indent(indent)
    return [`${baseIndent}.${id}${pseudo} {`, ...properties, `${baseIndent}}`].join('\n')
  }
  const classDef = wrapClass(properties, 0)
  const mediasDef = medias.map(({ query, properties, pseudoSelectors }) => {
    const rule = [`${query} {`, wrapClass(properties, 2)].join('\n')
    const sels = pseudoSelectors.map(({ properties, pseudoSelector }) => wrapClass(properties, 2, pseudoSelector))
    return [rule, ...sels, '}'].join('\n')
  })
  const selectorsDef = pseudoSelectors.map(({ properties, pseudoSelector }) => wrapClass(properties, 0, pseudoSelector))
  const name = `${classes.join(' ')} ${id}`.trim()

  cache.store(className, {
    name,
    previousArgs: args,
    indexRules: null,
    definitions: { mediasDef, selectorsDef, classDef }
  })

  return { name, className }
}

export function memo(klass) {
  cache.memoize(klass)
  return klass
}

export function toString({ name }) {
  return name
}
