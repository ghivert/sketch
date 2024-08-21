import * as React from 'react'

const styledFns = {}

export function styledFn(tag, defaultValue) {
  if (styledFns[tag]) return styledFns[tag]
  let value = defaultValue.bind({})
  value.displayName = `Sketch.Styled(${tag})`
  styledFns[tag] ??= value
  return value
}

export function extract(props) {
  const { styles, as, ...props_ } = props
  return [as, styles, props_]
}

export function assign(props, fieldName, value) {
  return { ...props, [fieldName]: value }
}

export function dumpStyles(style, content) {
  if (style.innerHTML === content) return
  style.innerHTML = content
}

export function useInsertionEffect(setup) {
  return React.useInsertionEffect(() => {
    setup()
  })
}

export function createStyleTag() {
  const style = document.createElement('style')
  document.head.appendChild(style)
  return style
}
