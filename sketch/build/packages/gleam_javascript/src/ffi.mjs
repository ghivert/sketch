import { Ok, Error } from "./gleam.mjs";
import {
  UndefinedType,
  ObjectType,
  BooleanType,
  NumberType,
  BigIntType,
  StringType,
  SymbolType,
  FunctionType,
} from "./gleam/javascript.mjs";

export function toArray(list) {
  return list.toArray();
}

export function map(thing, fn) {
  return thing.map(fn);
}

export function length(thing) {
  return thing.length;
}

export function reduce(thing, acc, fn) {
  return thing.reduce(fn, acc);
}

export function reduceRight(thing, acc, fn) {
  return thing.reduceRight(fn, acc);
}

export function index(thing, index) {
  return index in thing ? new Ok(thing[index]) : new Error(undefined);
}

export function object_from_entries(entries) {
  return Object.fromEntries(entries);
}

export function type_of(value) {
  switch (typeof value) {
    case "undefined":
      return new UndefinedType();
    case "object":
      return new ObjectType();
    case "boolean":
      return new BooleanType();
    case "number":
      return new NumberType();
    case "bigint":
      return new BigIntType();
    case "string":
      return new StringType();
    case "symbol":
      return new SymbolType();
    case "function":
      return new FunctionType();
    default:
      throw new globalThis.Error(`Unexpected typeof ${typeof value}`);
  }
}

export function get_symbol(name) {
  return Symbol.for(name);
}

// A wrapper around a promise to prevent `Promise<Promise<T>>` collapsing into
// `Promise<T>`.
class PromiseLayer {
  constructor(promise) {
    this.promise = promise;
  }

  static wrap(value) {
    return value instanceof Promise ? new PromiseLayer(value) : value;
  }

  static unwrap(value) {
    return value instanceof PromiseLayer ? value.promise : value;
  }
}

export function newPromise(executor) {
  return new Promise((resolve) =>
    executor((value) => {
      resolve(PromiseLayer.wrap(value));
    })
  );
}

export function resolve(value) {
  return Promise.resolve(PromiseLayer.wrap(value));
}

export function then(promise, fn) {
  return promise.then((value) => fn(PromiseLayer.unwrap(value)));
}

export function map_promise(promise, fn) {
  return promise.then((value) =>
    PromiseLayer.wrap(fn(PromiseLayer.unwrap(value)))
  );
}

export function rescue(promise, fn) {
  return promise.catch((error) => fn(error));
}

class Reference {
  constructor(value) {
    this.value = value;
  }
}

export function dereference(reference) {
  return reference.value;
}

export function make_reference(value) {
  return new Reference(value);
}

export function set_reference(ref, value) {
  let previous = ref.value;
  ref.value = value;
  return previous;
}

export function reference_equal(a,b) {
  return a === b
}

export function all_promises(...promises) {
  if (promises.length === 1) {
    return Promise.all(promises[0]);
  } else {
    return Promise.all(promises);
  }
}

export function map_new() {
  return new Map();
}

export function map_set(map, key, value) {
  return map.set(key, value);
}

export function map_get(map, key) {
  if (map.has(key)) {
    return new Ok(map.get(key));
  }
  return new Error(undefined);
}

export function map_size(map) {
  return map.size;
}
