export const _hole = a => {
  console.log("==[ HOLE ]=================================================================")
  console.log(a)
  throw new Error("HOLE")
}

export const realCatchException = (Left) => (Right) => (callback) => {
    try{
        var res = callback();
        return Right(res);
    }catch(e){
        return Left("error");
    }
}