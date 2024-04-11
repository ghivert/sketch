import { cache } from './cache.ffi.mjs'
import * as errors from './errors.ffi.mjs'
import * as helpers from './helpers.ffi.mjs'

// The Style data structure being a recursive data, computeProperties traverse
// the data structure and collect the properties with their context.
function computeProperties(rawProperties, indent = 2) {
  const properties = rawProperties.toArray()
  const init = { properties: [], medias: [], classes: [], pseudoSelectors: [], indent }
  return properties.reduce((accumulator, property) => {
    switch (helpers.determineStyleType(property)) {
      case 'compose': {
        const classes = [...accumulator.classes, property.class_name]
        return { ...accumulator, classes }
      }
      case 'property': {
        const cssProperty = helpers.computeProperty(accumulator.indent, property)
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

// Compute classes by using the class definitions, and by wrapping them in the
// correct class declarations, to be CSS compliant.
function computeClasses(id, computedProperties) {
  const { properties, medias, classes, pseudoSelectors } = computedProperties
  const classDef = helpers.wrapClass(id, properties, 0)
  const mediasDef = medias.map(({ query, properties, pseudoSelectors }) => {
    const sels = pseudoSelectors.map(p => helpers.wrapClass(id, p.properties, 2, p.pseudoSelector))
    return [`${query} {`, helpers.wrapClass(id, properties, 2), ...sels, '}'].join('\n')
  })
  const selectorsDef = pseudoSelectors.map(p => helpers.wrapClass(id, p.properties, 0, p.pseudoSelector))
  const name = `${classes.join(' ')} ${id}`.trim()
  return { classDef, mediasDef, selectorsDef, name }
}

// classId is optional
export function compileClass(styles, classId) {
  // Happens when prepare has not run before.
  if (!cache) {
    errors.warn.setup()
    // I'm not sure what to do here. Right now, it just conforms to the type system.
    return { name: "", className: "" }
  }

  // Search for already compiled class. Class can be already compiled and did
  // not change since last paint, or can be memoized.
  // If class already compiled, return directly the class.
  const className = classId ?? helpers.getFunctionName()
  const content = cache.persist(className)
  if (content) return content

  // The class is not found in the cache, or is different from the previous
  // class. It should be rebuild.
  const id = helpers.uid(className)
  const computedProperties = computeProperties(styles)
  const { name, ...definitions } = computeClasses(id, computedProperties)
  cache.store(className, { name, definitions, previousStyles: styles, indexRules: null })
  return { name, className }
}

// Memoize the class definitions in the cache.
// Once memoized, it's impossible to un-memoize a class.
export function memo(klass) {
  if (!cache) {
    errors.warn.setup()
  } else {
    cache.memoize(klass)
  }
  return klass
}

// Extract the name of the Class type, which is an opaque type for
// the type { name: string, className: string }
export function toString({ name }) {
  return name
}

export function updateLustre(application, viewMapper) {
  return application.withFields({
    view: viewMapper(application.view),
  })
}
