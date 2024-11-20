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

export function scrollTo(id) {
  const node = document.getElementById(id)
  const nav = document.getElementById('navbar')
  if (!node || !nav) return
  const nodeRect = node.getBoundingClientRect()
  const navRect = nav.getBoundingClientRect()
  const top = nodeRect.top - navRect.bottom - 18
  window.scrollTo({ top, behavior: 'smooth' })
}
