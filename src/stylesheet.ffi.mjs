import * as options from './craft/options.mjs'

export class StyleSheet {
  static for(type) {
    switch (options.stylesheet_to_string(type)) {
      case 'node': return new NodeStyleSheet()
      case 'browser' : return new BrowserStyleSheet()
    }
  }
}

class NodeStyleSheet {
  #styleElement
  #rules
  #index

  constructor() {
    this.#styleElement = document.createElement('style')
    this.#rules = new Map()
    this.#index = 0
    this.#styleElement.setAttribute('className', 'craft-stylesheet')
    document.head.appendChild(this.#styleElement)
  }

  insertRule(definition) {
    const index = this.#index++
    this.#rules.set(index, definition)
    console.log(definition)
    return index
  }

  deleteRule(index) {
    this.#rules.delete(index)
  }

  render() {
    const rules = [...this.#rules.values()].join('\n\n')
    this.#styleElement.innerHTML = rules
  }
}

class BrowserStyleSheet {
  #styleElement

  constructor() {
    this.#styleElement = document.createElement('style')
    this.#styleElement.setAttribute('className', 'craft-stylesheet')
    document.head.appendChild(this.#styleElement)
    if (!this.#styleElement.sheet) throw new Error('StyleSheet not found, styled cannot be used.')
  }

  insertRule(definition) {
    return this.#styleElement.sheet.insertRule(definition)
  }

  deleteRule(index) {
    return this.#styleElement.sheet.deleteRule(index)
  }

  render() {}
}
