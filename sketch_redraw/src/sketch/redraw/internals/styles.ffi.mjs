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
const hookStyleds = new Map()

export function cache(tag, defaultValue) {
  if (styleds[tag]) return styleds[tag]
  let value = defaultValue.bind({})
  value.displayName = `Sketch.Styled(${tag})`
  styleds[tag] ??= value
  return value
}

/** Hooked styled components must be unique per call site, otherwise React
 * reuses the same fiber for two different `styles` functions with potentially
 * different hooks sequences, and raises a hooks error. Gleam closures are
 * recreated at each render, so their identity is unstable, but their source
 * text is stable and identical for a given call site. Two call sites with the
 * exact same source run the exact same hooks, so sharing the component between
 * them is safe. If the Gleam closures are different between render (source to
 * source), then it's better to recreate a component on the fly, instead of
 * introducting bugs in the hooks. */
export function hookCache(tag, styles, defaultValue) {
  const key = `${tag}\0${styles.toString()}`
  let value = hookStyleds.get(key)
  if (value) return value
  value = defaultValue.bind({})
  value.displayName = `Sketch.Styled(${tag})`
  hookStyleds.set(key, value)
  return value
}

export function extractFrom(props) {
  const { styles, as, ...props_ } = props
  return [as, styles, props_]
}
