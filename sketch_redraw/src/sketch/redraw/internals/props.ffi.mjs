export function append(props, fieldName, value) {
  const prop = props[fieldName]
  const value_ = prop ? `${prop} ${value}` : value
  return { ...props, [fieldName]: value_ }
}
