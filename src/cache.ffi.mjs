import * as helpers from './helpers.ffi.mjs'

export let cache

const createStyleSheet = () => {
  const styleElement = document.createElement('style')
  styleElement.setAttribute('className', 'craft-stylesheet')
  document.head.appendChild(styleElement)
  if (!styleElement.sheet) throw new Error('StyleSheet not found, styled cannot be used.')
  return styleElement.sheet
}

export class Cache {
  #memoCache
  #activeCache
  #passiveCache
  #stylesheet

  constructor() {
    this.#memoCache = new Map()
    this.#activeCache = new Map()
    this.#passiveCache = new Map()
    this.#stylesheet = createStyleSheet()
  }

  prepare() {
    this.#passiveCache = this.#activeCache
    this.#activeCache = new Map()
  }

  diff() {
    const keys = new Set()
    for (const key of this.#activeCache) keys.add(key)
    for (const key of this.#passiveCache) keys.add(key)
    keys.forEach(key => {
      if (this.#activeCache.has(key)) {
        if (this.#activeCache[key].indexRules !== null) return
        return this.#insertStyles(this.#activeCache[key])
      }
      if (key in this.#passiveCache)
        return this.#deleteStyles(this.#passiveCache.get(key))
    })
  }

  persist(className, properties) {
    // If class is memoized, returns it. Should be most classes, when used with
    // craft.class() in order to maximize perfs.
    const memoizedContent = this.#memoCache.get(className)
    if (memoizedContent) return { name: memoizedContent.name, className }

    // In this branch, it should be craft.variable().
    // If class is already in use during this paint, returns it.
    const newContent = this.#activeCache.get(className)
    if (newContent) return { name: newContent.name, className }

    // If class was used during the previous paint, persist it to the current
    // paint if the properties are equal. If they're not, don't persist it in
    // order to get it wiped by the class collection later in the cycle.
    const oldContent = this.#passiveCache.get(className)
    if (oldContent && helpers.deepEqual(properties, previousProperties)) {
      this.#activeCache.set(className, oldContent)
      return { name: oldContent.name, className }
    }
  }

  // Store the newly computed class.
  // className: string
  // content: {
  //   name: string,
  //   previousArgs: List(Style),
  //   indexRules: number[] | null,
  //   definitions: { mediasDef: string, selectorsDef: string, classDef: string }
  //  }
  store(className, content) {
    this.#activeCache.set(className, content)
  }

  // Memoize a class in order to avoid multiple recomputations of properties.
  // It saves the content of the class right away in the browser, and will never
  // recompute them later.
  memoize({ className }) {
    const klass = this.#activeCache.get(className)
    this.#memoCache.set(className, klass)
    this.#activeCache.delete(className)
    if (klass.indexRules === null) {
      this.#insertStyles(klass)
    }
  }

  #insertStyles(klass) {
    const indexRules = []
    const { definitions } = klass
    const { mediasDef, selectorsDef, classDef } = definitions
    mediasDef.forEach(def => indexRules.push(this.#stylesheet.insertRule(def)))
    selectorsDef.forEach(def => indexRules.push(this.#stylesheet.insertRule(def)))
    indexRules.push(this.#stylesheet.insertRule(classDef))
    klass.indexRules = indexRules
  }

  #deleteStyles(klass) {
    klass.indexRules.forEach(indexRule => {
      this.#stylesheet.deleteRule(indexRule)
    })
  }
}


export function createCache() {
  cache = new Cache()
  return cache
}

export function prepareCache(cache) {
  cache.prepare()
}

export function renderCache(cache) {
  cache.diff()
}
