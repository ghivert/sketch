export function createNode() {
  const style = document.createElement('style')
  document.head.appendChild(style)
  return style
}

export function dump(style, content) {
  if (style.innerHTML === content) return
  style.innerHTML = content
}

const styleds = {}
const hookStyleds = {}

export function cache(tag, defaultValue) {
  if (styleds[tag]) return styleds[tag]
  let value = defaultValue.bind({})
  value.displayName = `Sketch.Styled(${tag})`
  styleds[tag] ??= value
  return value
}

export function hookCache(tag, defaultValue) {
  if (hookStyleds[tag]) return hookStyleds[tag]
  let value = defaultValue.bind({})
  value.displayName = `Sketch.Styled(${tag})`
  hookStyleds[tag] ??= value
  return value
}

export function extractFrom(props) {
  const { styles, as, ...props_ } = props
  return [as, styles, props_]
}
