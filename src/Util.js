export const fromHomogenousRecordToTupleArray_ = tuple => r => {
  // iterate through items of object
  const entries = Object.entries(r);
  let arr = [];
  entries.forEach(([k, v]) => arr.push(tuple(k, v)))
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