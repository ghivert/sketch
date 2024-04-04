export function indent(size) {
  const space = ' '
  return space.repeat(size)
}

export const uid = (function() {
  let id = 0
  return function() {
    const index = (id++).toString().padStart(4, '0')
    return `css-${index}`
  }
})

export function getFunctionName() {
  const error = new Error()
  if (!error.stack) throw new Error('Unable to find the stacktrace and to infer the className')
  const stack = error.stack ?? ''
  return stack.split('\n').slice(1, 5).join('\n')
}

export function deepEqual(args, previousArgs) {
  const constants = ['string', 'number', 'boolean']
  if (constants.includes(typeof args) || constants.includes(typeof previousArgs)) return args === previousArgs
  for (const value in args) {
    if (!(value in previousArgs)) return false
    const isSame = deepEqual(args[value], previousArgs[value])
    if (!isSame) return false
  }
  return true
}
