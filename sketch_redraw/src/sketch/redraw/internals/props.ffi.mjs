export function append(props, fieldName, value) {
  return { ...props, [fieldName]: value }
}
