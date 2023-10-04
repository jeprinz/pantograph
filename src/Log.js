export const _log = tag => x => k => {
  Terminal.log(`--[ ${tag} ]---------------------------------------------------`)
  Terminal.log(x)
  return k({})
}