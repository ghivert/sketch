import * as helpers from './helpers.ffi.mjs'
import * as error from './sketch/error.mjs'
import { stylesheet_to_string } from './sketch/options.mjs'
import * as gleam from './gleam.mjs'
import { StyleSheet } from './stylesheet.ffi.mjs'

export let cache

/**
 * The cache maintains a structure similar to a VDOM, but for CSS. It tries to
 * works on the more optimized way, by using two different cache, to store the
 * current state, and the old state, and apply a diff correctly.
 * The way it works is to first call the `prepare` function, and then to add
 * add style by using the `store` function. To avoid doing the hard work twice,
 * the function `persist` will try to get the style in the old cache, and
 * transfer it to the new cache, to make sure it will not be destroyed in the
 * diff.
 * It uses a virtual StyleSheet, in which it's possible to insert or delete rules
 * and allow to build the real stylesheet once the diff is made.
 */
export class Cache {
  #memoCache
  #activeCache
  #passiveCache
  #stylesheet

  constructor(options) {
    this.#memoCache = new Map()
    this.#activeCache = new Map()
    this.#passiveCache = new Map()
    this.#stylesheet = StyleSheet.for(options.stylesheet)
  }

  static create(options) {
    const stylesheetType = stylesheet_to_string(options.stylesheet)
    const isBrowserOnly = ['document', 'shadow-root'].includes(stylesheetType)
    if (isBrowserOnly && !helpers.isBrowser())
      return new gleam.Error(new error.NotABrowser())
    return new Cache(options)
  }

  prepare() {
    this.#passiveCache = this.#activeCache
    this.#activeCache = new Map()
  }

  // Compute the predefined classes C = (Keys(Old) ∩ Keys(New))
  // Remove the keys defined by Keys(Old) - C.
  // Insert the keys defined by Keys(New) - C.
  diff() {
    const keys = new Set()
    for (const key of this.#activeCache.keys()) keys.add(key)
    for (const key of this.#passiveCache.keys()) keys.add(key)
    keys.forEach((key) => {
      if (this.#activeCache.has(key)) {
        const klass = this.#activeCache.get(key)
        if (klass?.indexRules !== null) return
        return this.#insertStyles(klass)
      }
      if (this.#passiveCache.has(key))
        return this.#deleteStyles(this.#passiveCache.get(key))
    })
    this.#stylesheet.render()
  }

  persist(className, properties) {
    // If class is memoized, returns it. Should be most classes, when used with
    // sketch.class() in order to maximize perfs.
    const memoizedContent = this.#memoCache.get(className)
    if (memoizedContent) return { name: memoizedContent.name, className }

    // In this branch, it should be sketch.variable().
    // If class is already in use during this paint, returns it.
    const newContent = this.#activeCache.get(className)
    if (newContent) return { name: newContent.name, className }

    // If class was used during the previous paint, persist it to the current
    // paint if the properties are equal. If they're not, don't persist it in
    // order to get it wiped by the class collection later in the cycle.
    const oldContent = this.#passiveCache.get(className)
    if (
      oldContent &&
      helpers.deepEqual(properties, oldContent.previousStyles)
    ) {
      this.#activeCache.set(className, oldContent)
      return { name: oldContent.name, className }
    }
  }

  // Store the newly computed class.
  // className: string
  // content: {
  //   name: string,
  //   previousStyles: List(Style),
  //   indexRules: number[] | null,
  //   definitions: { medias_def: string, selectors_def: string, class_def: string },
  // }
  store(className, content) {
    this.#activeCache.set(className, content)
  }

  // Memoize a class in order to avoid multiple recomputations of properties.
  // It saves the content of the class right away in the browser, and will never
  // recompute them later.
  memoize({ className }) {
    if (this.#memoCache.has(className)) return
    const klass = this.#activeCache.get(className)
    this.#memoCache.set(className, klass)
    this.#activeCache.delete(className)
    if (klass.indexRules === null) {
      this.#insertStyles(klass)
    }
  }

  // Insert the styles in the stylesheet.
  // It inserts medias, selectors and index rules. It inserts first the rule,
  // then the selectors, and then the media, to respect the usual order in a
  // standard CSS sheet, and to respect precedence of the styles.
  #insertStyles(klass) {
    const indexRules = []
    const { definitions } = klass
    indexRules.push(this.#stylesheet.insertRule(definitions.class_def))
    for (const def of definitions.selectors_def)
      indexRules.push(this.#stylesheet.insertRule(def))
    for (const def of definitions.medias_def)
      indexRules.push(this.#stylesheet.insertRule(def))
    klass.indexRules = indexRules
  }

  #deleteStyles(klass) {
    klass.indexRules?.forEach((indexRule) => {
      this.#stylesheet.deleteRule(indexRule)
    })
  }
}

export function createCache(options) {
  const newCache = Cache.create(options)
  if (newCache instanceof Cache) cache = newCache
  return new gleam.Ok(newCache)
}

export function prepareCache(cache_) {
  cache = cache_
  cache.prepare()
}

export function renderCache(cache) {
  cache.diff()
  return new gleam.Ok(null)
}
