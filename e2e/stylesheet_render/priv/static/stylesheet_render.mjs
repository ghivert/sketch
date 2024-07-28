// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label) => label in fields ? fields[label] : this[label]
    );
    return new this.constructor(...properties);
  }
};
var List = class {
  static fromArray(array3, tail) {
    let t = tail || new Empty();
    for (let i = array3.length - 1; i >= 0; --i) {
      t = new NonEmpty(array3[i], t);
    }
    return t;
  }
  [Symbol.iterator]() {
    return new ListIterator(this);
  }
  toArray() {
    return [...this];
  }
  // @internal
  atLeastLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return true;
      desired--;
    }
    return desired <= 0;
  }
  // @internal
  hasLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return false;
      desired--;
    }
    return desired === 0;
  }
  countLength() {
    let length3 = 0;
    for (let _ of this)
      length3++;
    return length3;
  }
};
function prepend(element3, tail) {
  return new NonEmpty(element3, tail);
}
function toList(elements, tail) {
  return List.fromArray(elements, tail);
}
var ListIterator = class {
  #current;
  constructor(current) {
    this.#current = current;
  }
  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
};
var Empty = class extends List {
};
var NonEmpty = class extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
};
var BitArray = class _BitArray {
  constructor(buffer) {
    if (!(buffer instanceof Uint8Array)) {
      throw "BitArray can only be constructed from a Uint8Array";
    }
    this.buffer = buffer;
  }
  // @internal
  get length() {
    return this.buffer.length;
  }
  // @internal
  byteAt(index3) {
    return this.buffer[index3];
  }
  // @internal
  floatAt(index3) {
    return byteArrayToFloat(this.buffer.slice(index3, index3 + 8));
  }
  // @internal
  intFromSlice(start4, end) {
    return byteArrayToInt(this.buffer.slice(start4, end));
  }
  // @internal
  binaryFromSlice(start4, end) {
    return new _BitArray(this.buffer.slice(start4, end));
  }
  // @internal
  sliceAfter(index3) {
    return new _BitArray(this.buffer.slice(index3));
  }
};
var UtfCodepoint = class {
  constructor(value3) {
    this.value = value3;
  }
};
function byteArrayToInt(byteArray) {
  byteArray = byteArray.reverse();
  let value3 = 0;
  for (let i = byteArray.length - 1; i >= 0; i--) {
    value3 = value3 * 256 + byteArray[i];
  }
  return value3;
}
function byteArrayToFloat(byteArray) {
  return new Float64Array(byteArray.reverse().buffer)[0];
}
var Result = class _Result extends CustomType {
  // @internal
  static isResult(data) {
    return data instanceof _Result;
  }
};
var Ok = class extends Result {
  constructor(value3) {
    super();
    this[0] = value3;
  }
  // @internal
  isOk() {
    return true;
  }
};
var Error = class extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  // @internal
  isOk() {
    return false;
  }
};
function isEqual(x, y) {
  let values2 = [x, y];
  while (values2.length) {
    let a = values2.pop();
    let b = values2.pop();
    if (a === b)
      continue;
    if (!isObject(a) || !isObject(b))
      return false;
    let unequal = !structurallyCompatibleObjects(a, b) || unequalDates(a, b) || unequalBuffers(a, b) || unequalArrays(a, b) || unequalMaps(a, b) || unequalSets(a, b) || unequalRegExps(a, b);
    if (unequal)
      return false;
    const proto = Object.getPrototypeOf(a);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a.equals(b))
          continue;
        else
          return false;
      } catch {
      }
    }
    let [keys2, get3] = getters(a);
    for (let k of keys2(a)) {
      values2.push(get3(a, k), get3(b, k));
    }
  }
  return true;
}
function getters(object3) {
  if (object3 instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object3 instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}
function unequalDates(a, b) {
  return a instanceof Date && (a > b || a < b);
}
function unequalBuffers(a, b) {
  return a.buffer instanceof ArrayBuffer && a.BYTES_PER_ELEMENT && !(a.byteLength === b.byteLength && a.every((n, i) => n === b[i]));
}
function unequalArrays(a, b) {
  return Array.isArray(a) && a.length !== b.length;
}
function unequalMaps(a, b) {
  return a instanceof Map && a.size !== b.size;
}
function unequalSets(a, b) {
  return a instanceof Set && (a.size != b.size || [...a].some((e) => !b.has(e)));
}
function unequalRegExps(a, b) {
  return a instanceof RegExp && (a.source !== b.source || a.flags !== b.flags);
}
function isObject(a) {
  return typeof a === "object" && a !== null;
}
function structurallyCompatibleObjects(a, b) {
  if (typeof a !== "object" && typeof b !== "object" && (!a || !b))
    return false;
  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a instanceof c))
    return false;
  return a.constructor === b.constructor;
}
function remainderInt(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a % b;
  }
}
function makeError(variant, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.module = module;
  error.line = line;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}

// build/dev/javascript/gleam_stdlib/gleam/option.mjs
var Some = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var None = class extends CustomType {
};
function unwrap(option, default$) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return default$;
  }
}
function map(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return new Some(fun(x));
  } else {
    return new None();
  }
}

// build/dev/javascript/gleam_stdlib/dict.mjs
var referenceMap = /* @__PURE__ */ new WeakMap();
var tempDataView = new DataView(new ArrayBuffer(8));
var referenceUID = 0;
function hashByReference(o) {
  const known = referenceMap.get(o);
  if (known !== void 0) {
    return known;
  }
  const hash = referenceUID++;
  if (referenceUID === 2147483647) {
    referenceUID = 0;
  }
  referenceMap.set(o, hash);
  return hash;
}
function hashMerge(a, b) {
  return a ^ b + 2654435769 + (a << 6) + (a >> 2) | 0;
}
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0; i < len; i++) {
    hash = Math.imul(31, hash) + s.charCodeAt(i) | 0;
  }
  return hash;
}
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(73244475, i >> 16 ^ i) ^ j;
}
function hashBigInt(n) {
  return hashString(n.toString());
}
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      const code = o.hashCode(o);
      if (typeof code === "number") {
        return code;
      }
    } catch {
    }
  }
  if (o instanceof Promise || o instanceof WeakSet || o instanceof WeakMap) {
    return hashByReference(o);
  }
  if (o instanceof Date) {
    return hashNumber(o.getTime());
  }
  let h = 0;
  if (o instanceof ArrayBuffer) {
    o = new Uint8Array(o);
  }
  if (Array.isArray(o) || o instanceof Uint8Array) {
    for (let i = 0; i < o.length; i++) {
      h = Math.imul(31, h) + getHash(o[i]) | 0;
    }
  } else if (o instanceof Set) {
    o.forEach((v) => {
      h = h + getHash(v) | 0;
    });
  } else if (o instanceof Map) {
    o.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
  } else {
    const keys2 = Object.keys(o);
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      const v = o[k];
      h = h + hashMerge(getHash(v), hashString(k)) | 0;
    }
  }
  return h;
}
function getHash(u) {
  if (u === null)
    return 1108378658;
  if (u === void 0)
    return 1108378659;
  if (u === true)
    return 1108378657;
  if (u === false)
    return 1108378656;
  switch (typeof u) {
    case "number":
      return hashNumber(u);
    case "string":
      return hashString(u);
    case "bigint":
      return hashBigInt(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
    default:
      return 0;
  }
}
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;
var ENTRY = 0;
var ARRAY_NODE = 1;
var INDEX_NODE = 2;
var COLLISION_NODE = 3;
var EMPTY = {
  type: INDEX_NODE,
  bitmap: 0,
  array: []
};
function mask(hash, shift) {
  return hash >>> shift & MASK;
}
function bitpos(hash, shift) {
  return 1 << mask(hash, shift);
}
function bitcount(x) {
  x -= x >> 1 & 1431655765;
  x = (x & 858993459) + (x >> 2 & 858993459);
  x = x + (x >> 4) & 252645135;
  x += x >> 8;
  x += x >> 16;
  return x & 127;
}
function index(bitmap, bit) {
  return bitcount(bitmap & bit - 1);
}
function cloneAndSet(arr, at, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0; i < len; ++i) {
    out[i] = arr[i];
  }
  out[at] = val;
  return out;
}
function spliceIn(arr, at, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function spliceOut(arr, at) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  ++i;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function createNode(shift, key1, val1, key2hash, key2, val2) {
  const key1hash = getHash(key1);
  if (key1hash === key2hash) {
    return {
      type: COLLISION_NODE,
      hash: key1hash,
      array: [
        { type: ENTRY, k: key1, v: val1 },
        { type: ENTRY, k: key2, v: val2 }
      ]
    };
  }
  const addedLeaf = { val: false };
  return assoc(
    assocIndex(EMPTY, shift, key1hash, key1, val1, addedLeaf),
    shift,
    key2hash,
    key2,
    val2,
    addedLeaf
  );
}
function assoc(root3, shift, hash, key2, val, addedLeaf) {
  switch (root3.type) {
    case ARRAY_NODE:
      return assocArray(root3, shift, hash, key2, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root3, shift, hash, key2, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root3, shift, hash, key2, val, addedLeaf);
  }
}
function assocArray(root3, shift, hash, key2, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node2 = root3.array[idx];
  if (node2 === void 0) {
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root3.size + 1,
      array: cloneAndSet(root3.array, idx, { type: ENTRY, k: key2, v: val })
    };
  }
  if (node2.type === ENTRY) {
    if (isEqual(key2, node2.k)) {
      if (val === node2.v) {
        return root3;
      }
      return {
        type: ARRAY_NODE,
        size: root3.size,
        array: cloneAndSet(root3.array, idx, {
          type: ENTRY,
          k: key2,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root3.size,
      array: cloneAndSet(
        root3.array,
        idx,
        createNode(shift + SHIFT, node2.k, node2.v, hash, key2, val)
      )
    };
  }
  const n = assoc(node2, shift + SHIFT, hash, key2, val, addedLeaf);
  if (n === node2) {
    return root3;
  }
  return {
    type: ARRAY_NODE,
    size: root3.size,
    array: cloneAndSet(root3.array, idx, n)
  };
}
function assocIndex(root3, shift, hash, key2, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root3.bitmap, bit);
  if ((root3.bitmap & bit) !== 0) {
    const node2 = root3.array[idx];
    if (node2.type !== ENTRY) {
      const n = assoc(node2, shift + SHIFT, hash, key2, val, addedLeaf);
      if (n === node2) {
        return root3;
      }
      return {
        type: INDEX_NODE,
        bitmap: root3.bitmap,
        array: cloneAndSet(root3.array, idx, n)
      };
    }
    const nodeKey = node2.k;
    if (isEqual(key2, nodeKey)) {
      if (val === node2.v) {
        return root3;
      }
      return {
        type: INDEX_NODE,
        bitmap: root3.bitmap,
        array: cloneAndSet(root3.array, idx, {
          type: ENTRY,
          k: key2,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root3.bitmap,
      array: cloneAndSet(
        root3.array,
        idx,
        createNode(shift + SHIFT, nodeKey, node2.v, hash, key2, val)
      )
    };
  } else {
    const n = root3.array.length;
    if (n >= MAX_INDEX_NODE) {
      const nodes = new Array(32);
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key2, val, addedLeaf);
      let j = 0;
      let bitmap = root3.bitmap;
      for (let i = 0; i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node2 = root3.array[j++];
          nodes[i] = node2;
        }
        bitmap = bitmap >>> 1;
      }
      return {
        type: ARRAY_NODE,
        size: n + 1,
        array: nodes
      };
    } else {
      const newArray = spliceIn(root3.array, idx, {
        type: ENTRY,
        k: key2,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root3.bitmap | bit,
        array: newArray
      };
    }
  }
}
function assocCollision(root3, shift, hash, key2, val, addedLeaf) {
  if (hash === root3.hash) {
    const idx = collisionIndexOf(root3, key2);
    if (idx !== -1) {
      const entry = root3.array[idx];
      if (entry.v === val) {
        return root3;
      }
      return {
        type: COLLISION_NODE,
        hash,
        array: cloneAndSet(root3.array, idx, { type: ENTRY, k: key2, v: val })
      };
    }
    const size = root3.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root3.array, size, { type: ENTRY, k: key2, v: val })
    };
  }
  return assoc(
    {
      type: INDEX_NODE,
      bitmap: bitpos(root3.hash, shift),
      array: [root3]
    },
    shift,
    hash,
    key2,
    val,
    addedLeaf
  );
}
function collisionIndexOf(root3, key2) {
  const size = root3.array.length;
  for (let i = 0; i < size; i++) {
    if (isEqual(key2, root3.array[i].k)) {
      return i;
    }
  }
  return -1;
}
function find(root3, shift, hash, key2) {
  switch (root3.type) {
    case ARRAY_NODE:
      return findArray(root3, shift, hash, key2);
    case INDEX_NODE:
      return findIndex(root3, shift, hash, key2);
    case COLLISION_NODE:
      return findCollision(root3, key2);
  }
}
function findArray(root3, shift, hash, key2) {
  const idx = mask(hash, shift);
  const node2 = root3.array[idx];
  if (node2 === void 0) {
    return void 0;
  }
  if (node2.type !== ENTRY) {
    return find(node2, shift + SHIFT, hash, key2);
  }
  if (isEqual(key2, node2.k)) {
    return node2;
  }
  return void 0;
}
function findIndex(root3, shift, hash, key2) {
  const bit = bitpos(hash, shift);
  if ((root3.bitmap & bit) === 0) {
    return void 0;
  }
  const idx = index(root3.bitmap, bit);
  const node2 = root3.array[idx];
  if (node2.type !== ENTRY) {
    return find(node2, shift + SHIFT, hash, key2);
  }
  if (isEqual(key2, node2.k)) {
    return node2;
  }
  return void 0;
}
function findCollision(root3, key2) {
  const idx = collisionIndexOf(root3, key2);
  if (idx < 0) {
    return void 0;
  }
  return root3.array[idx];
}
function without(root3, shift, hash, key2) {
  switch (root3.type) {
    case ARRAY_NODE:
      return withoutArray(root3, shift, hash, key2);
    case INDEX_NODE:
      return withoutIndex(root3, shift, hash, key2);
    case COLLISION_NODE:
      return withoutCollision(root3, key2);
  }
}
function withoutArray(root3, shift, hash, key2) {
  const idx = mask(hash, shift);
  const node2 = root3.array[idx];
  if (node2 === void 0) {
    return root3;
  }
  let n = void 0;
  if (node2.type === ENTRY) {
    if (!isEqual(node2.k, key2)) {
      return root3;
    }
  } else {
    n = without(node2, shift + SHIFT, hash, key2);
    if (n === node2) {
      return root3;
    }
  }
  if (n === void 0) {
    if (root3.size <= MIN_ARRAY_NODE) {
      const arr = root3.array;
      const out = new Array(root3.size - 1);
      let i = 0;
      let j = 0;
      let bitmap = 0;
      while (i < idx) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      ++i;
      while (i < arr.length) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      return {
        type: INDEX_NODE,
        bitmap,
        array: out
      };
    }
    return {
      type: ARRAY_NODE,
      size: root3.size - 1,
      array: cloneAndSet(root3.array, idx, n)
    };
  }
  return {
    type: ARRAY_NODE,
    size: root3.size,
    array: cloneAndSet(root3.array, idx, n)
  };
}
function withoutIndex(root3, shift, hash, key2) {
  const bit = bitpos(hash, shift);
  if ((root3.bitmap & bit) === 0) {
    return root3;
  }
  const idx = index(root3.bitmap, bit);
  const node2 = root3.array[idx];
  if (node2.type !== ENTRY) {
    const n = without(node2, shift + SHIFT, hash, key2);
    if (n === node2) {
      return root3;
    }
    if (n !== void 0) {
      return {
        type: INDEX_NODE,
        bitmap: root3.bitmap,
        array: cloneAndSet(root3.array, idx, n)
      };
    }
    if (root3.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root3.bitmap ^ bit,
      array: spliceOut(root3.array, idx)
    };
  }
  if (isEqual(key2, node2.k)) {
    if (root3.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root3.bitmap ^ bit,
      array: spliceOut(root3.array, idx)
    };
  }
  return root3;
}
function withoutCollision(root3, key2) {
  const idx = collisionIndexOf(root3, key2);
  if (idx < 0) {
    return root3;
  }
  if (root3.array.length === 1) {
    return void 0;
  }
  return {
    type: COLLISION_NODE,
    hash: root3.hash,
    array: spliceOut(root3.array, idx)
  };
}
function forEach(root3, fn) {
  if (root3 === void 0) {
    return;
  }
  const items = root3.array;
  const size = items.length;
  for (let i = 0; i < size; i++) {
    const item = items[i];
    if (item === void 0) {
      continue;
    }
    if (item.type === ENTRY) {
      fn(item.v, item.k);
      continue;
    }
    forEach(item, fn);
  }
}
var Dict = class _Dict {
  /**
   * @template V
   * @param {Record<string,V>} o
   * @returns {Dict<string,V>}
   */
  static fromObject(o) {
    const keys2 = Object.keys(o);
    let m = _Dict.new();
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      m = m.set(k, o[k]);
    }
    return m;
  }
  /**
   * @template K,V
   * @param {Map<K,V>} o
   * @returns {Dict<K,V>}
   */
  static fromMap(o) {
    let m = _Dict.new();
    o.forEach((v, k) => {
      m = m.set(k, v);
    });
    return m;
  }
  static new() {
    return new _Dict(void 0, 0);
  }
  /**
   * @param {undefined | Node<K,V>} root
   * @param {number} size
   */
  constructor(root3, size) {
    this.root = root3;
    this.size = size;
  }
  /**
   * @template NotFound
   * @param {K} key
   * @param {NotFound} notFound
   * @returns {NotFound | V}
   */
  get(key2, notFound) {
    if (this.root === void 0) {
      return notFound;
    }
    const found = find(this.root, 0, getHash(key2), key2);
    if (found === void 0) {
      return notFound;
    }
    return found.v;
  }
  /**
   * @param {K} key
   * @param {V} val
   * @returns {Dict<K,V>}
   */
  set(key2, val) {
    const addedLeaf = { val: false };
    const root3 = this.root === void 0 ? EMPTY : this.root;
    const newRoot = assoc(root3, 0, getHash(key2), key2, val, addedLeaf);
    if (newRoot === this.root) {
      return this;
    }
    return new _Dict(newRoot, addedLeaf.val ? this.size + 1 : this.size);
  }
  /**
   * @param {K} key
   * @returns {Dict<K,V>}
   */
  delete(key2) {
    if (this.root === void 0) {
      return this;
    }
    const newRoot = without(this.root, 0, getHash(key2), key2);
    if (newRoot === this.root) {
      return this;
    }
    if (newRoot === void 0) {
      return _Dict.new();
    }
    return new _Dict(newRoot, this.size - 1);
  }
  /**
   * @param {K} key
   * @returns {boolean}
   */
  has(key2) {
    if (this.root === void 0) {
      return false;
    }
    return find(this.root, 0, getHash(key2), key2) !== void 0;
  }
  /**
   * @returns {[K,V][]}
   */
  entries() {
    if (this.root === void 0) {
      return [];
    }
    const result = [];
    this.forEach((v, k) => result.push([k, v]));
    return result;
  }
  /**
   *
   * @param {(val:V,key:K)=>void} fn
   */
  forEach(fn) {
    forEach(this.root, fn);
  }
  hashCode() {
    let h = 0;
    this.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
    return h;
  }
  /**
   * @param {unknown} o
   * @returns {boolean}
   */
  equals(o) {
    if (!(o instanceof _Dict) || this.size !== o.size) {
      return false;
    }
    let equal = true;
    this.forEach((v, k) => {
      equal = equal && isEqual(o.get(k, !v), v);
    });
    return equal;
  }
};

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var Nil = void 0;
var NOT_FOUND = {};
function identity(x) {
  return x;
}
function to_string(term) {
  return term.toString();
}
function float_to_string(float3) {
  const string3 = float3.toString();
  if (string3.indexOf(".") >= 0) {
    return string3;
  } else {
    return string3 + ".0";
  }
}
function add(a, b) {
  return a + b;
}
function join(xs, separator) {
  const iterator = xs[Symbol.iterator]();
  let result = iterator.next().value || "";
  let current = iterator.next();
  while (!current.done) {
    result = result + separator + current.value;
    current = iterator.next();
  }
  return result;
}
function concat(xs) {
  let result = "";
  for (const x of xs) {
    result = result + x;
  }
  return result;
}
var unicode_whitespaces = [
  " ",
  // Space
  "	",
  // Horizontal tab
  "\n",
  // Line feed
  "\v",
  // Vertical tab
  "\f",
  // Form feed
  "\r",
  // Carriage return
  "\x85",
  // Next line
  "\u2028",
  // Line separator
  "\u2029"
  // Paragraph separator
].join();
var left_trim_regex = new RegExp(`^([${unicode_whitespaces}]*)`, "g");
var right_trim_regex = new RegExp(`([${unicode_whitespaces}]*)$`, "g");
function trim(string3) {
  return trim_left(trim_right(string3));
}
function trim_left(string3) {
  return string3.replace(left_trim_regex, "");
}
function trim_right(string3) {
  return string3.replace(right_trim_regex, "");
}
function new_map() {
  return Dict.new();
}
function map_to_list(map6) {
  return List.fromArray(map6.entries());
}
function map_get(map6, key2) {
  const value3 = map6.get(key2, NOT_FOUND);
  if (value3 === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value3);
}
function map_insert(key2, value3, map6) {
  return map6.set(key2, value3);
}
function inspect(v) {
  const t = typeof v;
  if (v === true)
    return "True";
  if (v === false)
    return "False";
  if (v === null)
    return "//js(null)";
  if (v === void 0)
    return "Nil";
  if (t === "string")
    return inspectString(v);
  if (t === "bigint" || t === "number")
    return v.toString();
  if (Array.isArray(v))
    return `#(${v.map(inspect).join(", ")})`;
  if (v instanceof List)
    return inspectList(v);
  if (v instanceof UtfCodepoint)
    return inspectUtfCodepoint(v);
  if (v instanceof BitArray)
    return inspectBitArray(v);
  if (v instanceof CustomType)
    return inspectCustomType(v);
  if (v instanceof Dict)
    return inspectDict(v);
  if (v instanceof Set)
    return `//js(Set(${[...v].map(inspect).join(", ")}))`;
  if (v instanceof RegExp)
    return `//js(${v})`;
  if (v instanceof Date)
    return `//js(Date("${v.toISOString()}"))`;
  if (v instanceof Function) {
    const args = [];
    for (const i of Array(v.length).keys())
      args.push(String.fromCharCode(i + 97));
    return `//fn(${args.join(", ")}) { ... }`;
  }
  return inspectObject(v);
}
function inspectString(str) {
  let new_str = '"';
  for (let i = 0; i < str.length; i++) {
    let char = str[i];
    switch (char) {
      case "\n":
        new_str += "\\n";
        break;
      case "\r":
        new_str += "\\r";
        break;
      case "	":
        new_str += "\\t";
        break;
      case "\f":
        new_str += "\\f";
        break;
      case "\\":
        new_str += "\\\\";
        break;
      case '"':
        new_str += '\\"';
        break;
      default:
        if (char < " " || char > "~" && char < "\xA0") {
          new_str += "\\u{" + char.charCodeAt(0).toString(16).toUpperCase().padStart(4, "0") + "}";
        } else {
          new_str += char;
        }
    }
  }
  new_str += '"';
  return new_str;
}
function inspectDict(map6) {
  let body2 = "dict.from_list([";
  let first3 = true;
  map6.forEach((value3, key2) => {
    if (!first3)
      body2 = body2 + ", ";
    body2 = body2 + "#(" + inspect(key2) + ", " + inspect(value3) + ")";
    first3 = false;
  });
  return body2 + "])";
}
function inspectObject(v) {
  const name = Object.getPrototypeOf(v)?.constructor?.name || "Object";
  const props = [];
  for (const k of Object.keys(v)) {
    props.push(`${inspect(k)}: ${inspect(v[k])}`);
  }
  const body2 = props.length ? " " + props.join(", ") + " " : "";
  const head = name === "Object" ? "" : name + " ";
  return `//js(${head}{${body2}})`;
}
function inspectCustomType(record) {
  const props = Object.keys(record).map((label) => {
    const value3 = inspect(record[label]);
    return isNaN(parseInt(label)) ? `${label}: ${value3}` : value3;
  }).join(", ");
  return props ? `${record.constructor.name}(${props})` : record.constructor.name;
}
function inspectList(list) {
  return `[${list.toArray().map(inspect).join(", ")}]`;
}
function inspectBitArray(bits) {
  return `<<${Array.from(bits.buffer).join(", ")}>>`;
}
function inspectUtfCodepoint(codepoint2) {
  return `//utfcodepoint(${String.fromCodePoint(codepoint2.value)})`;
}

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function new$() {
  return new_map();
}
function get(from2, get3) {
  return map_get(from2, get3);
}
function insert(dict, key2, value3) {
  return map_insert(key2, value3, dict);
}
function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest = remaining.tail;
      loop$remaining = rest;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function do_values_acc(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let x = list.head;
      let xs = list.tail;
      loop$list = xs;
      loop$acc = prepend(x[1], acc);
    }
  }
}
function do_values(dict) {
  let list_of_pairs = map_to_list(dict);
  return do_values_acc(list_of_pairs, toList([]));
}
function values(dict) {
  return do_values(dict);
}

// build/dev/javascript/gleam_stdlib/gleam/float.mjs
function to_string2(x) {
  return float_to_string(x);
}

// build/dev/javascript/gleam_stdlib/gleam/int.mjs
function to_string3(x) {
  return to_string(x);
}
function to_float(x) {
  return identity(x);
}

// build/dev/javascript/gleam_stdlib/gleam/pair.mjs
function first(pair) {
  let a = pair[0];
  return a;
}
function second(pair) {
  let a = pair[1];
  return a;
}
function map_second(pair, fun) {
  let a = pair[0];
  let b = pair[1];
  return [a, fun(b)];
}
function new$2(first3, second2) {
  return [first3, second2];
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
function do_reverse(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest$1 = remaining.tail;
      loop$remaining = rest$1;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function reverse(xs) {
  return do_reverse(xs, toList([]));
}
function is_empty(list) {
  return isEqual(list, toList([]));
}
function do_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else {
      let x = list.head;
      let xs = list.tail;
      loop$list = xs;
      loop$fun = fun;
      loop$acc = prepend(fun(x), acc);
    }
  }
}
function map2(list, fun) {
  return do_map(list, fun, toList([]));
}
function do_append(loop$first, loop$second) {
  while (true) {
    let first3 = loop$first;
    let second2 = loop$second;
    if (first3.hasLength(0)) {
      return second2;
    } else {
      let item = first3.head;
      let rest$1 = first3.tail;
      loop$first = rest$1;
      loop$second = prepend(item, second2);
    }
  }
}
function append(first3, second2) {
  return do_append(reverse(first3), second2);
}
function prepend2(list, item) {
  return prepend(item, list);
}
function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix.hasLength(0)) {
      return suffix;
    } else {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = prepend(first$1, suffix);
    }
  }
}
function do_concat(loop$lists, loop$acc) {
  while (true) {
    let lists = loop$lists;
    let acc = loop$acc;
    if (lists.hasLength(0)) {
      return reverse(acc);
    } else {
      let list = lists.head;
      let further_lists = lists.tail;
      loop$lists = further_lists;
      loop$acc = reverse_and_prepend(list, acc);
    }
  }
}
function concat2(lists) {
  return do_concat(lists, toList([]));
}
function flat_map(list, fun) {
  let _pipe = map2(list, fun);
  return concat2(_pipe);
}
function fold_right(list, initial, fun) {
  if (list.hasLength(0)) {
    return initial;
  } else {
    let x = list.head;
    let rest$1 = list.tail;
    return fun(fold_right(rest$1, initial, fun), x);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/iterator.mjs
var Stop = class extends CustomType {
};
var Continue2 = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Iterator = class extends CustomType {
  constructor(continuation) {
    super();
    this.continuation = continuation;
  }
};
var Next = class extends CustomType {
  constructor(element3, accumulator) {
    super();
    this.element = element3;
    this.accumulator = accumulator;
  }
};
function do_unfold(initial, f) {
  return () => {
    let $ = f(initial);
    if ($ instanceof Next) {
      let x = $.element;
      let acc = $.accumulator;
      return new Continue2(x, do_unfold(acc, f));
    } else {
      return new Stop();
    }
  };
}
function unfold(initial, f) {
  let _pipe = initial;
  let _pipe$1 = do_unfold(_pipe, f);
  return new Iterator(_pipe$1);
}
function repeatedly(f) {
  return unfold(void 0, (_) => {
    return new Next(f(), void 0);
  });
}
function repeat(x) {
  return repeatedly(() => {
    return x;
  });
}
function do_fold(loop$continuation, loop$f, loop$accumulator) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let accumulator = loop$accumulator;
    let $ = continuation();
    if ($ instanceof Continue2) {
      let elem = $[0];
      let next = $[1];
      loop$continuation = next;
      loop$f = f;
      loop$accumulator = f(accumulator, elem);
    } else {
      return accumulator;
    }
  }
}
function fold(iterator, initial, f) {
  let _pipe = iterator.continuation;
  return do_fold(_pipe, f, initial);
}
function to_list(iterator) {
  let _pipe = iterator;
  let _pipe$1 = fold(
    _pipe,
    toList([]),
    (acc, e) => {
      return prepend(e, acc);
    }
  );
  return reverse(_pipe$1);
}
function do_take(continuation, desired) {
  return () => {
    let $ = desired > 0;
    if (!$) {
      return new Stop();
    } else {
      let $1 = continuation();
      if ($1 instanceof Stop) {
        return new Stop();
      } else {
        let e = $1[0];
        let next = $1[1];
        return new Continue2(e, do_take(next, desired - 1));
      }
    }
  };
}
function take(iterator, desired) {
  let _pipe = iterator.continuation;
  let _pipe$1 = do_take(_pipe, desired);
  return new Iterator(_pipe$1);
}

// build/dev/javascript/gleam_stdlib/gleam/string_builder.mjs
function append_builder(builder, suffix) {
  return add(builder, suffix);
}
function from_strings(strings) {
  return concat(strings);
}
function from_string(string3) {
  return identity(string3);
}
function append2(builder, second2) {
  return append_builder(builder, from_string(second2));
}
function to_string4(builder) {
  return identity(builder);
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function append4(first3, second2) {
  let _pipe = first3;
  let _pipe$1 = from_string(_pipe);
  let _pipe$2 = append2(_pipe$1, second2);
  return to_string4(_pipe$2);
}
function concat3(strings) {
  let _pipe = strings;
  let _pipe$1 = from_strings(_pipe);
  return to_string4(_pipe$1);
}
function repeat2(string3, times) {
  let _pipe = repeat(string3);
  let _pipe$1 = take(_pipe, times);
  let _pipe$2 = to_list(_pipe$1);
  return concat3(_pipe$2);
}
function join2(strings, separator) {
  return join(strings, separator);
}
function trim2(string3) {
  return trim(string3);
}
function inspect2(term) {
  let _pipe = inspect(term);
  return to_string4(_pipe);
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic.mjs
function from(a) {
  return identity(a);
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/lustre/lustre/effect.mjs
var Effect = class extends CustomType {
  constructor(all) {
    super();
    this.all = all;
  }
};
function none() {
  return new Effect(toList([]));
}

// build/dev/javascript/lustre/lustre/internals/vdom.mjs
var Text = class extends CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
};
var Element = class extends CustomType {
  constructor(key2, namespace, tag, attrs, children, self_closing, void$) {
    super();
    this.key = key2;
    this.namespace = namespace;
    this.tag = tag;
    this.attrs = attrs;
    this.children = children;
    this.self_closing = self_closing;
    this.void = void$;
  }
};
var Fragment = class extends CustomType {
  constructor(elements, key2) {
    super();
    this.elements = elements;
    this.key = key2;
  }
};
var Attribute = class extends CustomType {
  constructor(x0, x1, as_property) {
    super();
    this[0] = x0;
    this[1] = x1;
    this.as_property = as_property;
  }
};
var Event = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute(name, value3) {
  return new Attribute(name, from(value3), false);
}
function property(name, value3) {
  return new Attribute(name, from(value3), true);
}
function on(name, handler) {
  return new Event("on" + name, handler);
}
function class$(name) {
  return attribute("class", name);
}
function disabled(is_disabled) {
  return property("disabled", is_disabled);
}

// build/dev/javascript/lustre/lustre/element.mjs
function element(tag, attrs, children) {
  if (tag === "area") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "base") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "br") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "col") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "embed") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "hr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "img") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "input") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "link") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "meta") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "param") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "source") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "track") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "wbr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else {
    return new Element("", "", tag, attrs, children, false, false);
  }
}
function text(content) {
  return new Text(content);
}
function none2() {
  return new Text("");
}
function flatten_fragment_elements(elements) {
  return fold_right(
    elements,
    toList([]),
    (new_elements, element3) => {
      if (element3 instanceof Fragment) {
        let fr_elements = element3.elements;
        return append(fr_elements, new_elements);
      } else {
        let el2 = element3;
        return prepend(el2, new_elements);
      }
    }
  );
}
function fragment(elements) {
  let _pipe = flatten_fragment_elements(elements);
  return new Fragment(_pipe, "");
}

// build/dev/javascript/lustre/lustre/internals/runtime.mjs
var Debug = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Dispatch = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Shutdown = class extends CustomType {
};
var ForceModel = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};

// build/dev/javascript/lustre/vdom.ffi.mjs
function morph(prev, next, dispatch, isComponent = false) {
  let out;
  let stack = [{ prev, next, parent: prev.parentNode }];
  while (stack.length) {
    let { prev: prev2, next: next2, parent } = stack.pop();
    if (next2.subtree !== void 0)
      next2 = next2.subtree();
    if (next2.content !== void 0) {
      if (!prev2) {
        const created = document.createTextNode(next2.content);
        parent.appendChild(created);
        out ??= created;
      } else if (prev2.nodeType === Node.TEXT_NODE) {
        if (prev2.textContent !== next2.content)
          prev2.textContent = next2.content;
        out ??= prev2;
      } else {
        const created = document.createTextNode(next2.content);
        parent.replaceChild(created, prev2);
        out ??= created;
      }
    } else if (next2.tag !== void 0) {
      const created = createElementNode({
        prev: prev2,
        next: next2,
        dispatch,
        stack,
        isComponent
      });
      if (!prev2) {
        parent.appendChild(created);
      } else if (prev2 !== created) {
        parent.replaceChild(created, prev2);
      }
      out ??= created;
    } else if (next2.elements !== void 0) {
      iterateElement(next2, (fragmentElement) => {
        stack.unshift({ prev: prev2, next: fragmentElement, parent });
        prev2 = prev2?.nextSibling;
      });
    } else if (next2.subtree !== void 0) {
      stack.push({ prev: prev2, next: next2, parent });
    }
  }
  return out;
}
function createElementNode({ prev, next, dispatch, stack }) {
  const namespace = next.namespace || "http://www.w3.org/1999/xhtml";
  const canMorph = prev && prev.nodeType === Node.ELEMENT_NODE && prev.localName === next.tag && prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");
  const el2 = canMorph ? prev : namespace ? document.createElementNS(namespace, next.tag) : document.createElement(next.tag);
  let handlersForEl;
  if (!registeredHandlers.has(el2)) {
    const emptyHandlers = /* @__PURE__ */ new Map();
    registeredHandlers.set(el2, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el2);
  }
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph ? new Set(Array.from(prev.attributes, (a) => a.name)) : null;
  let className = null;
  let style2 = null;
  let innerHTML = null;
  for (const attr of next.attrs) {
    const name = attr[0];
    const value3 = attr[1];
    if (attr.as_property) {
      if (el2[name] !== value3)
        el2[name] = value3;
      if (canMorph)
        prevAttributes.delete(name);
    } else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value3);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name.startsWith("data-lustre-on-")) {
      const eventName = name.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el2.setAttribute(name, value3);
    } else if (name === "class") {
      className = className === null ? value3 : className + " " + value3;
    } else if (name === "style") {
      style2 = style2 === null ? value3 : style2 + value3;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value3;
    } else {
      if (el2.getAttribute(name) !== value3)
        el2.setAttribute(name, value3);
      if (name === "value" || name === "selected")
        el2[name] = value3;
      if (canMorph)
        prevAttributes.delete(name);
    }
  }
  if (className !== null) {
    el2.setAttribute("class", className);
    if (canMorph)
      prevAttributes.delete("class");
  }
  if (style2 !== null) {
    el2.setAttribute("style", style2);
    if (canMorph)
      prevAttributes.delete("style");
  }
  if (canMorph) {
    for (const attr of prevAttributes) {
      el2.removeAttribute(attr);
    }
    for (const eventName of prevHandlers) {
      handlersForEl.delete(eventName);
      el2.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }
  if (next.key !== void 0 && next.key !== "") {
    el2.setAttribute("data-lustre-key", next.key);
  } else if (innerHTML !== null) {
    el2.innerHTML = innerHTML;
    return el2;
  }
  let prevChild = el2.firstChild;
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = next.children[Symbol.iterator]().next().value;
  if (canMorph && firstChild !== void 0 && // Explicit checks are more verbose but truthy checks force a bunch of comparisons
  // we don't care about: it's never gonna be a number etc.
  firstChild.key !== void 0 && firstChild.key !== "") {
    seenKeys = /* @__PURE__ */ new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next);
  }
  for (const child of next.children) {
    iterateElement(child, (currElement) => {
      if (currElement.key !== void 0 && seenKeys !== null) {
        prevChild = diffKeyedChild(
          prevChild,
          currElement,
          el2,
          stack,
          incomingKeyedChildren,
          keyedChildren,
          seenKeys
        );
      } else {
        stack.unshift({ prev: prevChild, next: currElement, parent: el2 });
        prevChild = prevChild?.nextSibling;
      }
    });
  }
  while (prevChild) {
    const next2 = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = next2;
  }
  return el2;
}
var registeredHandlers = /* @__PURE__ */ new WeakMap();
function lustreGenericEventHandler(event2) {
  const target2 = event2.currentTarget;
  if (!registeredHandlers.has(target2)) {
    target2.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  const handlersForEventTarget = registeredHandlers.get(target2);
  if (!handlersForEventTarget.has(event2.type)) {
    target2.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  handlersForEventTarget.get(event2.type)(event2);
}
function lustreServerEventHandler(event2) {
  const el2 = event2.currentTarget;
  const tag = el2.getAttribute(`data-lustre-on-${event2.type}`);
  const data = JSON.parse(el2.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el2.getAttribute("data-lustre-include") || "[]");
  switch (event2.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }
  return {
    tag,
    data: include.reduce(
      (data2, property3) => {
        const path = property3.split(".");
        for (let i = 0, o = data2, e = event2; i < path.length; i++) {
          if (i === path.length - 1) {
            o[path[i]] = e[path[i]];
          } else {
            o[path[i]] ??= {};
            e = e[path[i]];
            o = o[path[i]];
          }
        }
        return data2;
      },
      { data }
    )
  };
}
function getKeyedChildren(el2) {
  const keyedChildren = /* @__PURE__ */ new Map();
  if (el2) {
    for (const child of el2.children) {
      iterateElement(child, (currElement) => {
        const key2 = currElement?.key || currElement?.getAttribute?.("data-lustre-key");
        if (key2)
          keyedChildren.set(key2, currElement);
      });
    }
  }
  return keyedChildren;
}
function diffKeyedChild(prevChild, child, el2, stack, incomingKeyedChildren, keyedChildren, seenKeys) {
  while (prevChild && !incomingKeyedChildren.has(prevChild.getAttribute("data-lustre-key"))) {
    const nextChild = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = nextChild;
  }
  if (keyedChildren.size === 0) {
    iterateElement(child, (currChild) => {
      stack.unshift({ prev: prevChild, next: currChild, parent: el2 });
      prevChild = prevChild?.nextSibling;
    });
    return prevChild;
  }
  if (seenKeys.has(child.key)) {
    console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
    stack.unshift({ prev: null, next: child, parent: el2 });
    return prevChild;
  }
  seenKeys.add(child.key);
  const keyedChild = keyedChildren.get(child.key);
  if (!keyedChild && !prevChild) {
    stack.unshift({ prev: null, next: child, parent: el2 });
    return prevChild;
  }
  if (!keyedChild && prevChild !== null) {
    const placeholder = document.createTextNode("");
    el2.insertBefore(placeholder, prevChild);
    stack.unshift({ prev: placeholder, next: child, parent: el2 });
    return prevChild;
  }
  if (!keyedChild || keyedChild === prevChild) {
    stack.unshift({ prev: prevChild, next: child, parent: el2 });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  el2.insertBefore(keyedChild, prevChild);
  stack.unshift({ prev: keyedChild, next: child, parent: el2 });
  return prevChild;
}
function iterateElement(element3, processElement) {
  if (element3.elements !== void 0) {
    for (const currElement of element3.elements) {
      processElement(currElement);
    }
  } else {
    processElement(element3);
  }
}

// build/dev/javascript/lustre/client-runtime.ffi.mjs
var LustreClientApplication2 = class _LustreClientApplication {
  #root = null;
  #queue = [];
  #effects = [];
  #didUpdate = false;
  #isComponent = false;
  #model = null;
  #update = null;
  #view = null;
  static start(flags, selector, init2, update2, view2) {
    if (!is_browser())
      return new Error(new NotABrowser());
    const root3 = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root3)
      return new Error(new ElementNotFound(selector));
    const app2 = new _LustreClientApplication(init2(flags), update2, view2, root3);
    return new Ok((msg) => app2.send(msg));
  }
  constructor([model, effects], update2, view2, root3 = document.body, isComponent = false) {
    this.#model = model;
    this.#update = update2;
    this.#view = view2;
    this.#root = root3;
    this.#effects = effects.all.toArray();
    this.#didUpdate = true;
    this.#isComponent = isComponent;
    window.requestAnimationFrame(() => this.#tick());
  }
  send(action) {
    switch (true) {
      case action instanceof Dispatch: {
        this.#queue.push(action[0]);
        this.#tick();
        return;
      }
      case action instanceof Shutdown: {
        this.#shutdown();
        return;
      }
      case action instanceof Debug: {
        this.#debug(action[0]);
        return;
      }
      default:
        return;
    }
  }
  emit(event2, data) {
    this.#root.dispatchEvent(
      new CustomEvent(event2, {
        bubbles: true,
        detail: data,
        composed: true
      })
    );
  }
  #tick() {
    this.#flush_queue();
    if (this.#didUpdate) {
      const vdom = this.#view(this.#model);
      const dispatch = (handler) => (e) => {
        const result = handler(e);
        if (result instanceof Ok) {
          this.send(new Dispatch(result[0]));
        }
      };
      this.#didUpdate = false;
      this.#root = morph(this.#root, vdom, dispatch, this.#isComponent);
    }
  }
  #flush_queue(iterations = 0) {
    while (this.#queue.length) {
      const [next, effects] = this.#update(this.#model, this.#queue.shift());
      this.#didUpdate ||= this.#model !== next;
      this.#model = next;
      this.#effects = this.#effects.concat(effects.all.toArray());
    }
    while (this.#effects.length) {
      this.#effects.shift()(
        (msg) => this.send(new Dispatch(msg)),
        (event2, data) => this.emit(event2, data)
      );
    }
    if (this.#queue.length) {
      if (iterations < 5) {
        this.#flush_queue(++iterations);
      } else {
        window.requestAnimationFrame(() => this.#tick());
      }
    }
  }
  #debug(action) {
    switch (true) {
      case action instanceof ForceModel: {
        const vdom = this.#view(action[0]);
        const dispatch = (handler) => (e) => {
          const result = handler(e);
          if (result instanceof Ok) {
            this.send(new Dispatch(result[0]));
          }
        };
        this.#queue = [];
        this.#effects = [];
        this.#didUpdate = false;
        this.#root = morph(this.#root, vdom, dispatch, this.#isComponent);
      }
    }
  }
  #shutdown() {
    this.#root.remove();
    this.#root = null;
    this.#model = null;
    this.#queue = [];
    this.#effects = [];
    this.#didUpdate = false;
    this.#update = () => {
    };
    this.#view = () => {
    };
  }
};
var start = (app2, selector, flags) => LustreClientApplication2.start(
  flags,
  selector,
  app2.init,
  app2.update,
  app2.view
);
var is_browser = () => globalThis.window && window.document;

// build/dev/javascript/lustre/lustre.mjs
var App = class extends CustomType {
  constructor(init2, update2, view2, on_attribute_change) {
    super();
    this.init = init2;
    this.update = update2;
    this.view = view2;
    this.on_attribute_change = on_attribute_change;
  }
};
var ElementNotFound = class extends CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
};
var NotABrowser = class extends CustomType {
};
function application(init2, update2, view2) {
  return new App(init2, update2, view2, new None());
}
function simple(init2, update2, view2) {
  let init$1 = (flags) => {
    return [init2(flags), none()];
  };
  let update$1 = (model, msg) => {
    return [update2(model, msg), none()];
  };
  return application(init$1, update$1, view2);
}
function start3(app2, selector, flags) {
  return guard(
    !is_browser(),
    new Error(new NotABrowser()),
    () => {
      return start(app2, selector, flags);
    }
  );
}

// build/dev/javascript/lustre/lustre/event.mjs
function on2(name, handler) {
  return on(name, handler);
}
function on_click(msg) {
  return on2("click", (_) => {
    return new Ok(msg);
  });
}

// build/dev/javascript/sketch/sketch/internals/class.mjs
var Definitions = class extends CustomType {
  constructor(medias_def, selectors_def, class_def) {
    super();
    this.medias_def = medias_def;
    this.selectors_def = selectors_def;
    this.class_def = class_def;
  }
};
var Content = class extends CustomType {
  constructor(class_name4, class_id, definitions2, rules) {
    super();
    this.class_name = class_name4;
    this.class_id = class_id;
    this.definitions = definitions2;
    this.rules = rules;
  }
};
function class_name(class$4) {
  return class$4.class_name;
}
function definitions(class$4) {
  let $ = class$4.definitions;
  let medias = $.medias_def;
  let selectors = $.selectors_def;
  let class$1 = $.class_def;
  let _pipe = toList([toList([class$1]), selectors, medias]);
  return concat2(_pipe);
}
function create(class_name4, class_id, rules, definitions2) {
  return new Content(class_name4, class_id, definitions2, rules);
}

// build/dev/javascript/sketch/sketch/internals/string.mjs
function indent(indent2) {
  return repeat2(" ", indent2);
}
function wrap_class(id, properties, idt, pseudo) {
  let base_indent = indent(idt);
  let pseudo_ = unwrap(pseudo, "");
  let _pipe = prepend(
    base_indent + "." + id + pseudo_ + " {",
    properties
  );
  let _pipe$1 = join2(_pipe, "\n");
  return append4(_pipe$1, "\n" + base_indent + "}");
}

// build/dev/javascript/sketch/xxhash.ffi.bin.mjs
var wasmBytes = "AGFzbQEAAAABMAhgA39/fwF/YAN/f38AYAJ/fwBgAX8Bf2ADf39+AX5gA35/fwF+YAJ/fgBgAX8BfgMLCgAAAgEDBAUGAQcFAwEAAQdVCQNtZW0CAAV4eGgzMgAABmluaXQzMgACCHVwZGF0ZTMyAAMIZGlnZXN0MzIABAV4eGg2NAAFBmluaXQ2NAAHCHVwZGF0ZTY0AAgIZGlnZXN0NjQACQr7FgryAQEEfyAAIAFqIQMgAUEQTwR/IANBEGshBiACQaiIjaECaiEDIAJBievQ0AdrIQQgAkHPjKKOBmohBQNAIAMgACgCAEH3lK+veGxqQQ13QbHz3fF5bCEDIAQgAEEEaiIAKAIAQfeUr694bGpBDXdBsfPd8XlsIQQgAiAAQQRqIgAoAgBB95Svr3hsakENd0Gx893xeWwhAiAFIABBBGoiACgCAEH3lK+veGxqQQ13QbHz3fF5bCEFIAYgAEEEaiIATw0ACyACQQx3IAVBEndqIARBB3dqIANBAXdqBSACQbHP2bIBagsgAWogACABQQ9xEAELkgEAIAEgAmohAgNAIAFBBGogAktFBEAgACABKAIAQb3cypV8bGpBEXdBr9bTvgJsIQAgAUEEaiEBDAELCwNAIAEgAk9FBEAgACABLQAAQbHP2bIBbGpBC3dBsfPd8XlsIQAgAUEBaiEBDAELCyAAIABBD3ZzQfeUr694bCIAQQ12IABzQb3cypV8bCIAQRB2IABzCz8AIABBCGogAUGoiI2hAmo2AgAgAEEMaiABQYnr0NAHazYCACAAQRBqIAE2AgAgAEEUaiABQc+Moo4GajYCAAvDBAEGfyABIAJqIQYgAEEYaiEEIABBKGooAgAhAyAAIAAoAgAgAmo2AgAgAEEEaiIFIAUoAgAgAkEQTyAAKAIAQRBPcnI2AgAgAiADakEQSQRAIAMgBGogASAC/AoAACAAQShqIAIgA2o2AgAPCyADBEAgAyAEaiABQRAgA2siAvwKAAAgAEEIaiIDIAMoAgAgBCgCAEH3lK+veGxqQQ13QbHz3fF5bDYCACAAQQxqIgMgAygCACAEQQRqKAIAQfeUr694bGpBDXdBsfPd8XlsNgIAIABBEGoiAyADKAIAIARBCGooAgBB95Svr3hsakENd0Gx893xeWw2AgAgAEEUaiIDIAMoAgAgBEEMaigCAEH3lK+veGxqQQ13QbHz3fF5bDYCACAAQShqQQA2AgAgASACaiEBCyABIAZBEGtNBEAgBkEQayEIIABBCGooAgAhAiAAQQxqKAIAIQMgAEEQaigCACEFIABBFGooAgAhBwNAIAIgASgCAEH3lK+veGxqQQ13QbHz3fF5bCECIAMgAUEEaiIBKAIAQfeUr694bGpBDXdBsfPd8XlsIQMgBSABQQRqIgEoAgBB95Svr3hsakENd0Gx893xeWwhBSAHIAFBBGoiASgCAEH3lK+veGxqQQ13QbHz3fF5bCEHIAggAUEEaiIBTw0ACyAAQQhqIAI2AgAgAEEMaiADNgIAIABBEGogBTYCACAAQRRqIAc2AgALIAEgBkkEQCAEIAEgBiABayIB/AoAACAAQShqIAE2AgALC2EBAX8gAEEQaigCACEBIABBBGooAgAEfyABQQx3IABBFGooAgBBEndqIABBDGooAgBBB3dqIABBCGooAgBBAXdqBSABQbHP2bIBagsgACgCAGogAEEYaiAAQShqKAIAEAEL/wMCA34BfyAAIAFqIQYgAUEgTwR+IAZBIGshBiACQtbrgu7q/Yn14AB8IQMgAkKxqazBrbjUpj19IQQgAkL56tDQ58mh5OEAfCEFA0AgAyAAKQMAQs/W077Sx6vZQn58Qh+JQoeVr6+Ytt6bnn9+IQMgBCAAQQhqIgApAwBCz9bTvtLHq9lCfnxCH4lCh5Wvr5i23puef34hBCACIABBCGoiACkDAELP1tO+0ser2UJ+fEIfiUKHla+vmLbem55/fiECIAUgAEEIaiIAKQMAQs/W077Sx6vZQn58Qh+JQoeVr6+Ytt6bnn9+IQUgBiAAQQhqIgBPDQALIAJCDIkgBUISiXwgBEIHiXwgA0IBiXwgA0LP1tO+0ser2UJ+Qh+JQoeVr6+Ytt6bnn9+hUKHla+vmLbem55/fkKdo7Xqg7GNivoAfSAEQs/W077Sx6vZQn5CH4lCh5Wvr5i23puef36FQoeVr6+Ytt6bnn9+Qp2jteqDsY2K+gB9IAJCz9bTvtLHq9lCfkIfiUKHla+vmLbem55/foVCh5Wvr5i23puef35CnaO16oOxjYr6AH0gBULP1tO+0ser2UJ+Qh+JQoeVr6+Ytt6bnn9+hUKHla+vmLbem55/fkKdo7Xqg7GNivoAfQUgAkLFz9my8eW66id8CyABrXwgACABQR9xEAYLhgIAIAEgAmohAgNAIAIgAUEIak8EQCABKQMAQs/W077Sx6vZQn5CH4lCh5Wvr5i23puef34gAIVCG4lCh5Wvr5i23puef35CnaO16oOxjYr6AH0hACABQQhqIQEMAQsLIAFBBGogAk0EQCAAIAE1AgBCh5Wvr5i23puef36FQheJQs/W077Sx6vZQn5C+fPd8Zn2masWfCEAIAFBBGohAQsDQCABIAJJBEAgACABMQAAQsXP2bLx5brqJ36FQguJQoeVr6+Ytt6bnn9+IQAgAUEBaiEBDAELCyAAIABCIYiFQs/W077Sx6vZQn4iACAAQh2IhUL5893xmfaZqxZ+IgAgAEIgiIULTQAgAEEIaiABQtbrgu7q/Yn14AB8NwMAIABBEGogAUKxqazBrbjUpj19NwMAIABBGGogATcDACAAQSBqIAFC+erQ0OfJoeThAHw3AwAL9AQCA38EfiABIAJqIQUgAEEoaiEEIABByABqKAIAIQMgACAAKQMAIAKtfDcDACACIANqQSBJBEAgAyAEaiABIAL8CgAAIABByABqIAIgA2o2AgAPCyADBEAgAyAEaiABQSAgA2siAvwKAAAgAEEIaiIDIAMpAwAgBCkDAELP1tO+0ser2UJ+fEIfiUKHla+vmLbem55/fjcDACAAQRBqIgMgAykDACAEQQhqKQMAQs/W077Sx6vZQn58Qh+JQoeVr6+Ytt6bnn9+NwMAIABBGGoiAyADKQMAIARBEGopAwBCz9bTvtLHq9lCfnxCH4lCh5Wvr5i23puef343AwAgAEEgaiIDIAMpAwAgBEEYaikDAELP1tO+0ser2UJ+fEIfiUKHla+vmLbem55/fjcDACAAQcgAakEANgIAIAEgAmohAQsgAUEgaiAFTQRAIAVBIGshAiAAQQhqKQMAIQYgAEEQaikDACEHIABBGGopAwAhCCAAQSBqKQMAIQkDQCAGIAEpAwBCz9bTvtLHq9lCfnxCH4lCh5Wvr5i23puef34hBiAHIAFBCGoiASkDAELP1tO+0ser2UJ+fEIfiUKHla+vmLbem55/fiEHIAggAUEIaiIBKQMAQs/W077Sx6vZQn58Qh+JQoeVr6+Ytt6bnn9+IQggCSABQQhqIgEpAwBCz9bTvtLHq9lCfnxCH4lCh5Wvr5i23puef34hCSACIAFBCGoiAU8NAAsgAEEIaiAGNwMAIABBEGogBzcDACAAQRhqIAg3AwAgAEEgaiAJNwMACyABIAVJBEAgBCABIAUgAWsiAfwKAAAgAEHIAGogATYCAAsLvAIBBX4gAEEYaikDACEBIAApAwAiAkIgWgR+IABBCGopAwAiA0IBiSAAQRBqKQMAIgRCB4l8IAFCDIkgAEEgaikDACIFQhKJfHwgA0LP1tO+0ser2UJ+Qh+JQoeVr6+Ytt6bnn9+hUKHla+vmLbem55/fkKdo7Xqg7GNivoAfSAEQs/W077Sx6vZQn5CH4lCh5Wvr5i23puef36FQoeVr6+Ytt6bnn9+Qp2jteqDsY2K+gB9IAFCz9bTvtLHq9lCfkIfiUKHla+vmLbem55/foVCh5Wvr5i23puef35CnaO16oOxjYr6AH0gBULP1tO+0ser2UJ+Qh+JQoeVr6+Ytt6bnn9+hUKHla+vmLbem55/fkKdo7Xqg7GNivoAfQUgAULFz9my8eW66id8CyACfCAAQShqIAJCH4OnEAYL";

// build/dev/javascript/sketch/xxhash.ffi.mjs
var u32_BYTES = 4;
var u64_BYTES = 8;
var XXH32_STATE_SIZE_BYTES = u32_BYTES + // total_len
u32_BYTES + // large_len
u32_BYTES * 4 + // Accumulator lanes
u32_BYTES * 4 + // Internal buffer
u32_BYTES + // memsize
u32_BYTES;
var XXH64_STATE_SIZE_BYTES = u64_BYTES + // total_len
u64_BYTES * 4 + // Accumulator lanes
u64_BYTES * 4 + // Internal buffer
u32_BYTES + // memsize
u32_BYTES + // reserved32
u64_BYTES;
function xxhash() {
  const bytes = Uint8Array.from(atob(wasmBytes), (c) => c.charCodeAt(0));
  const mod = new WebAssembly.Module(bytes);
  const {
    exports: {
      mem,
      xxh32,
      xxh64,
      init32,
      update32,
      digest32,
      init64,
      update64,
      digest64
    }
  } = new WebAssembly.Instance(mod);
  let memory = new Uint8Array(mem.buffer);
  function growMemory(length3, offset) {
    if (mem.buffer.byteLength < length3 + offset) {
      const extraPages = Math.ceil(
        // Wasm pages are spec'd to 64K
        (length3 + offset - mem.buffer.byteLength) / (64 * 1024)
      );
      mem.grow(extraPages);
      memory = new Uint8Array(mem.buffer);
    }
  }
  function create2(size, seed, init2, update2, digest, finalize) {
    growMemory(size);
    const state = new Uint8Array(size);
    memory.set(state);
    init2(0, seed);
    state.set(memory.slice(0, size));
    return {
      update(input) {
        memory.set(state);
        let length3;
        if (typeof input === "string") {
          growMemory(input.length * 3, size);
          length3 = encoder.encodeInto(input, memory.subarray(size)).written;
        } else {
          growMemory(input.byteLength, size);
          memory.set(input, size);
          length3 = input.byteLength;
        }
        update2(0, size, length3);
        state.set(memory.slice(0, size));
        return this;
      },
      digest() {
        memory.set(state);
        return finalize(digest(0));
      }
    };
  }
  function forceUnsigned32(i) {
    return i >>> 0;
  }
  const u64Max = 2n ** 64n - 1n;
  function forceUnsigned64(i) {
    return i & u64Max;
  }
  const encoder = new TextEncoder();
  const defaultSeed = 0;
  const defaultBigSeed = 0n;
  function h32(str, seed = defaultSeed) {
    growMemory(str.length * 3, 0);
    return forceUnsigned32(
      xxh32(0, encoder.encodeInto(str, memory).written, seed)
    );
  }
  function h64(str, seed = defaultBigSeed) {
    growMemory(str.length * 3, 0);
    return forceUnsigned64(
      xxh64(0, encoder.encodeInto(str, memory).written, seed)
    );
  }
  return {
    h32,
    h32ToString(str, seed = defaultSeed) {
      return h32(str, seed).toString(16).padStart(8, "0");
    },
    h32Raw(inputBuffer, seed = defaultSeed) {
      growMemory(inputBuffer.byteLength, 0);
      memory.set(inputBuffer);
      return forceUnsigned32(xxh32(0, inputBuffer.byteLength, seed));
    },
    create32(seed = defaultSeed) {
      return create2(
        XXH32_STATE_SIZE_BYTES,
        seed,
        init32,
        update32,
        digest32,
        forceUnsigned32
      );
    },
    h64,
    h64ToString(str, seed = defaultBigSeed) {
      return h64(str, seed).toString(16).padStart(16, "0");
    },
    h64Raw(inputBuffer, seed = defaultBigSeed) {
      growMemory(inputBuffer.byteLength, 0);
      memory.set(inputBuffer);
      return forceUnsigned64(xxh64(0, inputBuffer.byteLength, seed));
    },
    create64(seed = defaultBigSeed) {
      return create2(
        XXH64_STATE_SIZE_BYTES,
        seed,
        init64,
        update64,
        digest64,
        forceUnsigned64
      );
    }
  };
}
var hasher = xxhash();
function xxHash32(content) {
  return hasher.h32(content);
}

// build/dev/javascript/sketch/sketch/internals/style.mjs
var Class = class extends CustomType {
  constructor(string_representation, content) {
    super();
    this.string_representation = string_representation;
    this.content = content;
  }
};
var EphemeralCache = class extends CustomType {
  constructor(cache2) {
    super();
    this.cache = cache2;
  }
};
var PersistentCache = class extends CustomType {
  constructor(cache2, current_id) {
    super();
    this.cache = cache2;
    this.current_id = current_id;
  }
};
var ClassName = class extends CustomType {
  constructor(class_name4) {
    super();
    this.class_name = class_name4;
  }
};
var Media = class extends CustomType {
  constructor(query, styles) {
    super();
    this.query = query;
    this.styles = styles;
  }
};
var PseudoSelector = class extends CustomType {
  constructor(pseudo_selector2, styles) {
    super();
    this.pseudo_selector = pseudo_selector2;
    this.styles = styles;
  }
};
var Property = class extends CustomType {
  constructor(key2, value3, important2) {
    super();
    this.key = key2;
    this.value = value3;
    this.important = important2;
  }
};
var NoStyle = class extends CustomType {
};
var ComputedProperties = class extends CustomType {
  constructor(properties, medias, classes, pseudo_selectors, indent2) {
    super();
    this.properties = properties;
    this.medias = medias;
    this.classes = classes;
    this.pseudo_selectors = pseudo_selectors;
    this.indent = indent2;
  }
};
var MediaProperty = class extends CustomType {
  constructor(query, properties, pseudo_selectors) {
    super();
    this.query = query;
    this.properties = properties;
    this.pseudo_selectors = pseudo_selectors;
  }
};
var PseudoProperty = class extends CustomType {
  constructor(pseudo_selector2, properties) {
    super();
    this.pseudo_selector = pseudo_selector2;
    this.properties = properties;
  }
};
var ComputedClass = class extends CustomType {
  constructor(class_def, medias_def, selectors_def, name) {
    super();
    this.class_def = class_def;
    this.medias_def = medias_def;
    this.selectors_def = selectors_def;
    this.name = name;
  }
};
function persistent() {
  return new PersistentCache(new$(), 0);
}
function ephemeral() {
  return new EphemeralCache(new$());
}
function compute_property(indent2, key2, value3, important2) {
  let base_indent = indent(indent2);
  let important_ = (() => {
    if (important2) {
      return " !important";
    } else {
      return "";
    }
  })();
  return base_indent + key2 + ": " + value3 + important_ + ";";
}
function init_computed_properties(indent2) {
  return new ComputedProperties(
    toList([]),
    toList([]),
    toList([]),
    toList([]),
    indent2
  );
}
function handle_class_name(props, class_name4) {
  let classes = prepend(class_name4, props.classes);
  return props.withFields({ classes });
}
function handle_property(props, style2) {
  if (!(style2 instanceof Property)) {
    throw makeError(
      "assignment_no_match",
      "sketch/internals/style",
      124,
      "handle_property",
      "Assignment pattern did not match",
      { value: style2 }
    );
  }
  let key2 = style2.key;
  let value3 = style2.value;
  let important2 = style2.important;
  let css_property = compute_property(props.indent, key2, value3, important2);
  let properties = prepend(css_property, props.properties);
  return props.withFields({ properties });
}
function wrap_pseudo_selectors(id, indent2, pseudo_selectors) {
  return map2(
    pseudo_selectors,
    (p) => {
      return wrap_class(
        id,
        p.properties,
        indent2,
        new Some(p.pseudo_selector)
      );
    }
  );
}
function compute_classes(class_name4, computed_properties) {
  let properties = computed_properties.properties;
  let medias = computed_properties.medias;
  let classes = computed_properties.classes;
  let pseudo_selectors = computed_properties.pseudo_selectors;
  let class_def = wrap_class(
    class_name4,
    properties,
    0,
    new None()
  );
  let medias_def = map2(
    medias,
    (_use0) => {
      let query = _use0.query;
      let properties$1 = _use0.properties;
      let pseudo_selectors$1 = _use0.pseudo_selectors;
      let selectors_def2 = wrap_pseudo_selectors(
        class_name4,
        2,
        pseudo_selectors$1
      );
      let _pipe = toList([
        query + " {",
        wrap_class(class_name4, properties$1, 2, new None())
      ]);
      let _pipe$1 = ((_capture) => {
        return prepend2(toList([selectors_def2, toList(["}"])]), _capture);
      })(_pipe);
      let _pipe$2 = concat2(_pipe$1);
      return join2(_pipe$2, "\n");
    }
  );
  let selectors_def = wrap_pseudo_selectors(class_name4, 0, pseudo_selectors);
  let name = trim2(join2(classes, " ") + " " + class_name4);
  return new ComputedClass(class_def, medias_def, selectors_def, name);
}
function class$2(styles) {
  let string_representation = inspect2(styles);
  return new Class(string_representation, styles);
}
function render_cache_dict(cache2) {
  let _pipe = values(cache2);
  let _pipe$1 = flat_map(_pipe, definitions);
  return join2(_pipe$1, "\n\n");
}
function render(cache2) {
  if (cache2 instanceof EphemeralCache) {
    let cache$1 = cache2.cache;
    return render_cache_dict(cache$1);
  } else {
    let cache$1 = cache2.cache;
    return render_cache_dict(cache$1);
  }
}
function handle_media(cache2, props, style2) {
  if (!(style2 instanceof Media)) {
    throw makeError(
      "assignment_no_match",
      "sketch/internals/style",
      131,
      "handle_media",
      "Assignment pattern did not match",
      { value: style2 }
    );
  }
  let query = style2.query;
  let styles = style2.styles;
  let $ = compute_properties(cache2, styles, props.indent + 2);
  let cache$1 = $[0];
  let computed_props = $[1];
  let _pipe = new MediaProperty(
    query,
    computed_props.properties,
    computed_props.pseudo_selectors
  );
  let _pipe$1 = ((_capture) => {
    return prepend2(props.medias, _capture);
  })(
    _pipe
  );
  let _pipe$2 = ((m) => {
    return props.withFields({ medias: m });
  })(_pipe$1);
  return ((_capture) => {
    return new$2(cache$1, _capture);
  })(_pipe$2);
}
function compute_properties(cache2, properties, indent2) {
  let init2 = init_computed_properties(indent2);
  return fold_right(
    properties,
    [cache2, init2],
    (_use0, prop) => {
      let cache$1 = _use0[0];
      let acc = _use0[1];
      if (prop instanceof NoStyle) {
        return [cache$1, acc];
      } else if (prop instanceof Property) {
        return [cache$1, handle_property(acc, prop)];
      } else if (prop instanceof Media) {
        return handle_media(cache$1, acc, prop);
      } else if (prop instanceof PseudoSelector) {
        return handle_pseudo_selector(cache$1, acc, prop);
      } else {
        let class$1 = prop.class_name;
        let $ = class_name2(class$1, cache$1);
        let cache$2 = $[0];
        let class$22 = $[1];
        return [cache$2, handle_class_name(acc, class$22)];
      }
    }
  );
}
function handle_pseudo_selector(cache2, props, style2) {
  if (!(style2 instanceof PseudoSelector)) {
    throw makeError(
      "assignment_no_match",
      "sketch/internals/style",
      145,
      "handle_pseudo_selector",
      "Assignment pattern did not match",
      { value: style2 }
    );
  }
  let pseudo_selector2 = style2.pseudo_selector;
  let styles = style2.styles;
  let $ = compute_properties(cache2, styles, props.indent + 2);
  let cache$1 = $[0];
  let computed_props = $[1];
  let _pipe = new PseudoProperty(pseudo_selector2, computed_props.properties);
  let _pipe$1 = ((_capture) => {
    return prepend2(computed_props.pseudo_selectors, _capture);
  })(_pipe);
  let _pipe$2 = append(_pipe$1, props.pseudo_selectors);
  let _pipe$3 = ((p) => {
    return props.withFields({ pseudo_selectors: p });
  })(
    _pipe$2
  );
  return ((_capture) => {
    return new$2(cache$1, _capture);
  })(_pipe$3);
}
function compute_class(cache2, class$4) {
  let s = class$4.string_representation;
  let c = class$4.content;
  let $ = compute_properties(cache2, c, 2);
  let cache$1 = $[0];
  let properties = $[1];
  let class_id = (() => {
    if (cache$1 instanceof EphemeralCache) {
      return xxHash32(s);
    } else {
      let cid = cache$1.current_id;
      return cid;
    }
  })();
  let class_name$1 = "css-" + to_string3(class_id);
  let _pipe = compute_classes(class_name$1, properties);
  let _pipe$1 = ((c2) => {
    return create(
      c2.name,
      class_id,
      new None(),
      new Definitions(c2.medias_def, c2.selectors_def, c2.class_def)
    );
  })(_pipe);
  return ((class$5) => {
    let _pipe$2 = cache$1.cache;
    let _pipe$3 = insert(_pipe$2, s, class$5);
    let _pipe$4 = ((c2) => {
      if (cache$1 instanceof EphemeralCache) {
        return new EphemeralCache(c2);
      } else {
        return new PersistentCache(c2, class_id + 1);
      }
    })(_pipe$3);
    return new$2(_pipe$4, class$5);
  })(_pipe$1);
}
function class_name2(class$4, cache2) {
  let s = class$4.string_representation;
  let c = class$4.content;
  return guard(
    is_empty(c),
    [cache2, ""],
    () => {
      let $ = get(cache2.cache, s);
      if ($.isOk()) {
        let content = $[0];
        return [cache2, class_name(content)];
      } else {
        let _pipe = compute_class(cache2, class$4);
        return map_second(_pipe, class_name);
      }
    }
  );
}

// build/dev/javascript/sketch/sketch/size.mjs
var Px = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Pt = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Vh = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Vw = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Em = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Rem = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Lh = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Rlh = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Ch = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Pct = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
function px(value3) {
  return new Px(to_float(value3));
}
function percent(value3) {
  return new Pct(to_float(value3));
}
function rem(value3) {
  return new Rem(value3);
}
function ch(value3) {
  return new Ch(to_float(value3));
}
function to_string6(size) {
  if (size instanceof Px) {
    let value3 = size[0];
    return append4(to_string2(value3), "px");
  } else if (size instanceof Pt) {
    let value3 = size[0];
    return append4(to_string2(value3), "pt");
  } else if (size instanceof Pct) {
    let value3 = size[0];
    return append4(to_string2(value3), "%");
  } else if (size instanceof Vh) {
    let value3 = size[0];
    return append4(to_string2(value3), "vh");
  } else if (size instanceof Vw) {
    let value3 = size[0];
    return append4(to_string2(value3), "vw");
  } else if (size instanceof Em) {
    let value3 = size[0];
    return append4(to_string2(value3), "em");
  } else if (size instanceof Rem) {
    let value3 = size[0];
    return append4(to_string2(value3), "rem");
  } else if (size instanceof Lh) {
    let value3 = size[0];
    return append4(to_string2(value3), "lh");
  } else if (size instanceof Rlh) {
    let value3 = size[0];
    return append4(to_string2(value3), "rlh");
  } else {
    let value3 = size[0];
    return append4(to_string2(value3), "ch");
  }
}

// build/dev/javascript/sketch/sketch.mjs
var JsCache = class extends CustomType {
  constructor(cache2) {
    super();
    this.cache = cache2;
  }
};
var Ephemeral = class extends CustomType {
};
var Persistent = class extends CustomType {
};
function class$3(styles) {
  return class$2(styles);
}
function render2(cache2) {
  if (!(cache2 instanceof JsCache)) {
    throw makeError(
      "assignment_no_match",
      "sketch",
      53,
      "render",
      "Assignment pattern did not match",
      { value: cache2 }
    );
  }
  let cache$1 = cache2.cache;
  return render(cache$1);
}
function class_name3(class$4, cache2) {
  if (!(cache2 instanceof JsCache)) {
    throw makeError(
      "assignment_no_match",
      "sketch",
      62,
      "class_name",
      "Assignment pattern did not match",
      { value: cache2 }
    );
  }
  let cache$1 = cache2.cache;
  let $ = class_name2(class$4, cache$1);
  let cache$2 = $[0];
  let class_name$1 = $[1];
  return [new JsCache(cache$2), class_name$1];
}
function cache(strategy) {
  return new Ok(
    (() => {
      if (strategy instanceof Ephemeral) {
        return new JsCache(ephemeral());
      } else {
        return new JsCache(persistent());
      }
    })()
  );
}
function property2(field2, content) {
  return new Property(field2, content, false);
}
function align_items(align) {
  return property2("align-items", align);
}
function appearance(appearance2) {
  return property2("appearance", appearance2);
}
function background(background2) {
  return property2("background", background2);
}
function border(border2) {
  return property2("border", border2);
}
function border_radius(border_radius2) {
  return property2("border-radius", to_string6(border_radius2));
}
function color(color2) {
  return property2("color", color2);
}
function cursor(cursor2) {
  return property2("cursor", cursor2);
}
function display(display2) {
  return property2("display", display2);
}
function flex(flex2) {
  return property2("flex", flex2);
}
function flex_direction(flex_direction2) {
  return property2("flex-direction", flex_direction2);
}
function font_family(font_family2) {
  return property2("font-family", font_family2);
}
function font_size(font_size2) {
  return property2("font-size", to_string6(font_size2));
}
function font_weight(font_weight2) {
  return property2("font-weight", font_weight2);
}
function gap(gap2) {
  return property2("gap", to_string6(gap2));
}
function grid_area(grid_area2) {
  return property2("grid-area", grid_area2);
}
function grid_template_areas(grid_template_areas2) {
  return property2(
    "grid-template-areas",
    (() => {
      let _pipe = grid_template_areas2;
      let _pipe$1 = map2(
        _pipe,
        (content) => {
          return '"' + content + '"';
        }
      );
      return join2(_pipe$1, "\n");
    })()
  );
}
function grid_template_columns(grid_template_columns2) {
  return property2("grid-template-columns", grid_template_columns2);
}
function height(height2) {
  return property2("height", to_string6(height2));
}
function justify_content(justify) {
  return property2("justify-content", justify);
}
function line_height(line_height2) {
  return property2("line-height", line_height2);
}
function margin_(margin) {
  return property2("margin", margin);
}
function max_width(width2) {
  return property2("max-width", to_string6(width2));
}
function min_width(width2) {
  return property2("min-width", to_string6(width2));
}
function opacity(opacity2) {
  return property2("opacity", to_string2(opacity2));
}
function padding(padding2) {
  return property2("padding", to_string6(padding2));
}
function padding_(padding2) {
  return property2("padding", padding2);
}
function padding_bottom(padding2) {
  return property2("padding-bottom", to_string6(padding2));
}
function padding_left(padding2) {
  return property2("padding-left", to_string6(padding2));
}
function padding_top(padding2) {
  return property2("padding-top", to_string6(padding2));
}
function text_align(text_align2) {
  return property2("text-align", text_align2);
}
function transition(transition2) {
  return property2("transition", transition2);
}
function width(width2) {
  return property2("width", to_string6(width2));
}
function pseudo_selector(value3, styles) {
  return new PseudoSelector(value3, styles);
}
function hover(styles) {
  return pseudo_selector(":hover", styles);
}
function disabled2(styles) {
  return pseudo_selector(":disabled", styles);
}
function first_child(styles) {
  return pseudo_selector(":first-child", styles);
}
function important(style2) {
  if (style2 instanceof Property) {
    let key2 = style2.key;
    let value3 = style2.value;
    return new Property(key2, value3, true);
  } else {
    let any = style2;
    return any;
  }
}
function compose(class$4) {
  return new ClassName(class$4);
}

// build/dev/javascript/sketch_lustre/sketch_lustre.ffi.mjs
function wrap(current) {
  if (isPersistent(current))
    return { current };
  return current;
}
function set(variable, newValue) {
  if (!("current" in variable))
    return newValue;
  variable.current = newValue;
  return variable;
}
function get2(variable) {
  if ("current" in variable)
    return variable.current;
  return variable;
}
function isPersistent(cache2) {
  return "cache" in cache2 && "current_id" in cache2.cache;
}
function createCssStyleSheet(root3) {
  const stylesheet = new CSSStyleSheet();
  if (root3 && root3.adoptedStyleSheets) {
    root3.adoptedStyleSheets.push(stylesheet);
  } else {
    document.adoptedStyleSheets.push(stylesheet);
  }
  return stylesheet;
}
function setStylesheet(content, stylesheet) {
  stylesheet.replaceSync(content);
}

// build/dev/javascript/sketch_lustre/sketch/lustre/element.mjs
var Nothing = class extends CustomType {
};
var Text2 = class extends CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
};
var Fragment2 = class extends CustomType {
  constructor(key2, children) {
    super();
    this.key = key2;
    this.children = children;
  }
};
var Map3 = class extends CustomType {
  constructor(subtree) {
    super();
    this.subtree = subtree;
  }
};
var Element2 = class extends CustomType {
  constructor(key2, namespace, tag, attributes, children, styles) {
    super();
    this.key = key2;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.children = children;
    this.styles = styles;
  }
};
function text2(content) {
  return new Text2(content);
}
function element2(tag, attributes, children, styles) {
  return new Element2("", "", tag, attributes, children, styles);
}
function unstyled_children(cache2, children) {
  return fold_right(
    children,
    [cache2, toList([])],
    (acc, child) => {
      let cache$1 = acc[0];
      let children$1 = acc[1];
      let $ = unstyled(cache$1, child);
      let cache$2 = $[0];
      let child$1 = $[1];
      return [cache$2, prepend(child$1, children$1)];
    }
  );
}
function unstyled(loop$cache, loop$element) {
  while (true) {
    let cache2 = loop$cache;
    let element3 = loop$element;
    if (element3 instanceof Nothing) {
      return [cache2, none2()];
    } else if (element3 instanceof Text2) {
      let content = element3.content;
      return [cache2, text(content)];
    } else if (element3 instanceof Map3) {
      let subtree = element3.subtree;
      loop$cache = cache2;
      loop$element = subtree();
    } else if (element3 instanceof Fragment2) {
      let key2 = element3.key;
      let children = element3.children;
      let _pipe = unstyled_children(cache2, children);
      return map_second(
        _pipe,
        (node2) => {
          return new Fragment(node2, key2);
        }
      );
    } else {
      let key2 = element3.key;
      let namespace = element3.namespace;
      let tag = element3.tag;
      let attributes = element3.attributes;
      let children = element3.children;
      let styles = element3.styles;
      let class$4 = (() => {
        let $2 = is_empty(styles);
        if ($2) {
          return new None();
        } else {
          return new Some(class$3(styles));
        }
      })();
      let class$1 = map(
        class$4,
        (_capture) => {
          return class_name3(_capture, cache2);
        }
      );
      let class_name4 = map(class$1, second);
      let cache$1 = (() => {
        let _pipe = map(class$1, first);
        return unwrap(_pipe, cache2);
      })();
      let $ = unstyled_children(cache$1, children);
      let cache$2 = $[0];
      let children$1 = $[1];
      let attributes$1 = (() => {
        if (class_name4 instanceof None) {
          return attributes;
        } else {
          let class_name$1 = class_name4[0];
          let class_name$2 = class$(class_name$1);
          return prepend(class_name$2, attributes);
        }
      })();
      return [
        cache$2,
        (() => {
          let $1 = element(tag, attributes$1, children$1);
          if ($1 instanceof Element) {
            let t = $1.tag;
            let a = $1.attrs;
            let c = $1.children;
            let s = $1.self_closing;
            let v = $1.void;
            return new Element(key2, namespace, t, a, c, s, v);
          } else {
            let e = $1;
            return e;
          }
        })()
      ];
    }
  }
}

// build/dev/javascript/sketch_lustre/sketch/lustre.mjs
var Node2 = class extends CustomType {
};
var Document = class extends CustomType {
};
var Options = class extends CustomType {
  constructor(stylesheet) {
    super();
    this.stylesheet = stylesheet;
  }
};
var CssStyleSheet = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var NodeStyleSheet = class extends CustomType {
};
function to_stylesheet(options) {
  if (options instanceof Options && options.stylesheet instanceof Node2) {
    return new NodeStyleSheet();
  } else if (options instanceof Options && options.stylesheet instanceof Document) {
    return new CssStyleSheet(createCssStyleSheet());
  } else {
    let root$1 = options.stylesheet.root;
    return new CssStyleSheet(createCssStyleSheet(root$1));
  }
}
function root2(children) {
  return fragment(children);
}
function render_stylesheet(content, node2, stylesheet) {
  if (stylesheet instanceof NodeStyleSheet) {
    return root2(
      toList([
        element("style", toList([]), toList([text(content)])),
        node2
      ])
    );
  } else {
    let stylesheet$1 = stylesheet[0];
    setStylesheet(content, stylesheet$1);
    return node2;
  }
}
function compose2(options, view2, cache2) {
  let cache$1 = wrap(cache2);
  let stylesheet = to_stylesheet(options);
  return (model) => {
    let node$1 = view2(model);
    let $ = unstyled(get2(cache$1), node$1);
    let result = $[0];
    let node$2 = $[1];
    let content = render2(result);
    set(cache$1, result);
    return render_stylesheet(content, node$2, stylesheet);
  };
}
function node() {
  return new Options(new Node2());
}

// build/dev/javascript/sketch_lustre/sketch/lustre/element/html.mjs
function text3(content) {
  return text2(content);
}
function br_(attributes) {
  return element2("br", attributes, toList([]), toList([]));
}
function button(attributes, children, styles) {
  return element2("button", attributes, children, styles);
}
function div(attributes, children, styles) {
  return element2("div", attributes, children, styles);
}
function div_(attributes, children) {
  return element2("div", attributes, children, toList([]));
}
function main(attributes, children, styles) {
  return element2("main", attributes, children, styles);
}

// build/dev/javascript/shared_view/components.mjs
function card() {
  return class$3(
    toList([
      display("flex"),
      flex_direction("column"),
      border_radius(px(20)),
      padding(px(20)),
      font_size(rem(3)),
      font_weight("900")
    ])
  );
}
function card_body() {
  return class$3(
    toList([
      flex("1"),
      font_family("Lexend"),
      font_weight("600"),
      font_size(rem(1)),
      line_height("normal")
    ])
  );
}
function body(attrs, children) {
  return div(
    attrs,
    children,
    toList([
      line_height("0.75"),
      display("grid"),
      gap(px(10)),
      padding(px(10)),
      grid_template_columns("repeat(2, 1fr)"),
      grid_template_areas(
        toList(["topbar topbar", "headline headline", "counter showcase"])
      ),
      max_width(px(1200)),
      margin_("auto")
    ])
  );
}
function topbar(attrs, children) {
  return div(
    attrs,
    children,
    toList([
      display("flex"),
      grid_area("topbar"),
      font_size(rem(1.2)),
      padding_left(px(20)),
      padding_top(px(5))
    ])
  );
}
function headline(value3, attrs, children) {
  let background2 = (() => {
    let $ = remainderInt(value3, 2) === 1;
    if ($) {
      return "var(--atomic-tangerine)";
    } else {
      return "var(--periwinkle)";
    }
  })();
  return main(
    attrs,
    children,
    toList([
      grid_area("headline"),
      background(background2),
      text_align("center"),
      (() => {
        let _pipe = padding(px(120));
        return important(_pipe);
      })(),
      gap(px(20)),
      transition("all .2s"),
      compose(card())
    ])
  );
}
function headline_subtitle(attrs, children) {
  return div(
    attrs,
    children,
    toList([font_size(rem(1.2)), font_weight("normal")])
  );
}
function headline_emphasize(attrs, children) {
  return div(
    attrs,
    children,
    toList([font_size(rem(3)), font_weight("900")])
  );
}
function counter(attrs, children) {
  return div(
    attrs,
    children,
    toList([
      grid_area("counter"),
      background("var(--aquamarine)"),
      compose(card())
    ])
  );
}
function counter_subtitle(attrs, children) {
  return div(
    attrs,
    children,
    toList([
      font_weight("normal"),
      font_size(rem(0.9)),
      first_child(toList([padding_top(px(5))]))
    ])
  );
}
function button2(attrs, children) {
  return button(
    attrs,
    children,
    toList([
      appearance("none"),
      border("none"),
      font_family("Lexend"),
      background("black"),
      color("white"),
      border_radius(px(5)),
      padding_("5px 20px"),
      font_size(rem(1.2)),
      font_weight("600"),
      min_width(px(100)),
      cursor("pointer"),
      disabled2(
        toList([opacity(0.3), cursor("not-allowed")])
      ),
      hover(toList([background("#333")]))
    ])
  );
}
function value2(attrs, children) {
  return div(
    attrs,
    children,
    toList([
      background("var(--turquoise)"),
      height(percent(100)),
      display("flex"),
      align_items("center"),
      justify_content("center"),
      border_radius(px(5))
    ])
  );
}
function value_content(attrs, children) {
  return div(
    attrs,
    children,
    toList([width(ch(7)), text_align("center")])
  );
}
function showcase(attrs, children) {
  return div(
    attrs,
    children,
    toList([
      grid_area("showcase"),
      background("var(--turquoise)"),
      compose(card())
    ])
  );
}
function counter_body(attrs, children) {
  return div(
    attrs,
    children,
    toList([compose(card_body()), padding_bottom(px(40))])
  );
}
function counter_body_title(attrs, children) {
  return div(
    attrs,
    children,
    toList([
      display("flex"),
      flex_direction("column"),
      gap(px(20))
    ])
  );
}
function counter_counter(attrs, children) {
  return div(
    attrs,
    children,
    toList([
      display("grid"),
      align_items("center"),
      grid_template_columns("repeat(3, auto)"),
      justify_content("start"),
      font_size(rem(1.5)),
      gap(px(10))
    ])
  );
}
function showcase_body(attrs, children) {
  return div(
    attrs,
    children,
    toList([compose(card_body()), opacity(0.5)])
  );
}
function card_title(attrs, children) {
  return div(attrs, children, toList([]));
}

// build/dev/javascript/shared_view/shared_view.mjs
var Increment = class extends CustomType {
};
var Decrement = class extends CustomType {
};
function update(model, msg) {
  if (msg instanceof Increment) {
    return model + 1;
  } else {
    return model - 1;
  }
}
function view_counter_description() {
  let use_counter = "Use the counter, and see the site changing with the model!";
  let now_edit = "Now, try to edit the code to see the modifications live!";
  return div_(
    toList([]),
    toList([
      text3("Counter"),
      counter_subtitle(toList([]), toList([text3(use_counter)])),
      counter_subtitle(toList([]), toList([text3(now_edit)]))
    ])
  );
}
function view_counter(model) {
  let disabled3 = disabled(model <= 0);
  let model$1 = to_string3(model);
  return counter_counter(
    toList([]),
    toList([
      button2(
        toList([on_click(new Decrement()), disabled3]),
        toList([text3("-")])
      ),
      value2(
        toList([]),
        toList([
          value_content(toList([]), toList([text3(model$1)]))
        ])
      ),
      button2(
        toList([on_click(new Increment())]),
        toList([text3("+")])
      )
    ])
  );
}
function view(model) {
  return body(
    toList([]),
    toList([
      topbar(toList([]), toList([text3("Sketch")])),
      headline(
        model,
        toList([]),
        toList([
          headline_subtitle(
            toList([]),
            toList([text3("CSS-in-Gleam")])
          ),
          headline_emphasize(
            toList([]),
            toList([
              text3("Improve your CSS"),
              br_(toList([])),
              text3("with Sketch")
            ])
          )
        ])
      ),
      counter(
        toList([]),
        toList([
          counter_body(
            toList([]),
            toList([
              counter_body_title(
                toList([]),
                toList([view_counter_description(), view_counter(model)])
              )
            ])
          ),
          card_title(
            toList([]),
            toList([text3("See it in action")])
          )
        ])
      ),
      showcase(
        toList([]),
        toList([
          showcase_body(
            toList([]),
            toList([text3("Coming soon\u2026")])
          ),
          card_title(toList([]), toList([text3("Showcase")]))
        ])
      )
    ])
  );
}
function app() {
  let $ = cache(new Persistent());
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "shared_view",
      19,
      "app",
      "Assignment pattern did not match",
      { value: $ }
    );
  }
  let cache2 = $[0];
  let _pipe = node();
  let _pipe$1 = compose2(_pipe, view, cache2);
  return ((_capture) => {
    return simple((_) => {
      return 0;
    }, update, _capture);
  })(_pipe$1);
}

// build/dev/javascript/stylesheet_render/stylesheet_render.mjs
function main2() {
  let _pipe = app();
  return start3(_pipe, "#app", void 0);
}

// build/.lustre/entry.mjs
main2();
