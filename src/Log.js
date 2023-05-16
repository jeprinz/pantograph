export const _log = tag => x => k => {
  console.log(`--[ ${tag} ]---------------------------------------------------`)
  console.log(x)
  return k({})
}