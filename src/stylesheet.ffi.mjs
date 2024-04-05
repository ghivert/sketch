import * as options from './craft/options.mjs'

export class StyleSheet {
  static for(type) {
    switch (options.stylesheet_to_string(type)) {
      case 'node': return new NodeStyleSheet()
      case 'browser' : return new BrowserStyleSheet()
    }
  }
}

/** Create an abstract StyleSheet, responding to the same interface as CSSStyleSheet
 * with buildRules added, to get the corresponding StyleSheet as string. */
class AbstractSheet {
  #rules
  #index

  constructor() {
    this.#rules = new Map()
    this.#index = 0
  }

  insertRule(definition) {
    const index = this.#index++
    this.#rules.set(index, definition)
    return index
  }

  deleteRule(index) {
    this.#rules.delete(index)
  }

  buildRules() {
    return [...this.#rules.values()].join('\n\n')
  }
}

/** Create a virtual StyleSheet, responding to the same interface as CSSStyleSheet
 * with render added, and push the stylesheet in a node in DOM. */
class NodeStyleSheet extends AbstractSheet {
  #styleElement

  constructor() {
    super()
    this.#styleElement = document.createElement('style')
    this.#styleElement.setAttribute('class', 'craft-stylesheet')
    document.head.appendChild(this.#styleElement)
  }

  render() {
    const rules = this.buildRules()
    this.#styleElement.innerHTML = rules
  }
}

/** Create a virtual StyleSheet, responding to the same interface as CSSStyleSheet
 * with render added, and push the stylesheet in a node in DOM. */
class BrowserStyleSheet extends AbstractSheet {
  #styleElement

  constructor() {
    super()
    this.#styleElement = new CSSStyleSheet()
    document.adoptedStyleSheets.push(this.#styleElement)
  }

  render() {
    const rules = this.buildRules()
    this.#styleElement.replaceSync(rules)
  }
}
