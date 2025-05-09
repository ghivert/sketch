export function createDocument() {
  const stylesheet = new CSSStyleSheet()
  document.adoptedStyleSheets.push(stylesheet)
  return stylesheet
}

export function createRoot(root) {
  const stylesheet = new CSSStyleSheet()
  if (root && root.adoptedStyleSheets) root.adoptedStyleSheets.push(stylesheet)
  return stylesheet
}

export function replaceSync(content, stylesheet) {
  stylesheet.replaceSync(content)
}
