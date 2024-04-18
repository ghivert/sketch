import { cache } from './cache.ffi.mjs'
import * as errors from './errors.ffi.mjs'
import * as helpers from './helpers.ffi.mjs'
import * as sketch from './sketch/internals/style.mjs'

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
  const computedProperties = sketch.compute_properties(styles, 2)
  const { name, ...definitions } = sketch.compute_classes(id, computedProperties)
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
