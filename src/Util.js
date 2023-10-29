const util = (function() {
  try {
    return req === undefined ? undefined : req("util");
  } catch(e) {
    return undefined;
  }
})();

export const fromHomogenousRecordToTupleArray_ = tuple => r => {
  // iterate through items of object
  const entries = Object.entries(r);
  let arr = [];
  entries.forEach(([k, v]) => arr.push(tuple(k)(v)));
  return arr;
}

export function unsafeInsert(l) {
  return function (a) {
    return function (rec) {
      rec[l] = a;
      return rec;
    };
  };
}

export function showObject(x) {
  // node only recurses two levels into an object before printing
  // "[object]" for further objects when using console.log()
  if (util !== undefined) {
    return util.inspect(x, { depth: null, colors: true });
  } else {
    return x;
  }
}