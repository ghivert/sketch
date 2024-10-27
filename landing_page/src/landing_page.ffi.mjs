import hljs from 'highlight.js/lib/core'

export function setTimeout(...args) {
  return window.setTimeout(...args)
}

export function clearTimeout(...args) {
  return window.clearTimeout(...args)
}

export function clipboardCopy(text) {
  return navigator.clipboard.writeText(text)
}

export function highlight(code) {
  return hljs.highlight(code, { language: 'gleam' }).value
}

export function highlightCss(code) {
  return hljs.highlight(code, { language: 'css' }).value
}

export function highlightXml(code) {
  return hljs.highlight(code, { language: 'xml' }).value
}
