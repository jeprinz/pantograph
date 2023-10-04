export const _hole = a => {
  Terminal.log("==[ HOLE ]=================================================================")
  Terminal.log(a)
  throw new Error("HOLE")
}