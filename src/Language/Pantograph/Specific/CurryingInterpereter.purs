module Language.Pantograph.Specific.CurryingInterpereter where

import Prelude
import Language.Pantograph.Generic.Grammar as Grammar
import Data.Expr
import Data.List (List(..), (:))
import Data.List as List
import Bug (bug)
import Language.Pantograph.Specific.Currying
import Data.Tuple.Nested
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Either (Either(..))
import Data.Either as Either
import Util as Util
import Data.Int (pow)
import Data.Lazy (Lazy, defer, force)

data Value = IntVal Int | BoolVal Boolean | ListVal (List Value) | FunVal (Value -> Either Error Value)

eqValue :: Value -> Value -> Boolean
eqValue v1 v2 = case v1 /\ v2 of
    IntVal x /\ IntVal y -> x == y
    BoolVal x /\ BoolVal y -> x == y
    ListVal x /\ ListVal y -> List.all (\x -> x) (List.zipWith eqValue x y)
    _ /\ _ -> false

assertValInt :: Value -> Int
assertValInt = case _ of
    IntVal x -> x
    _ -> bug "assertValint failed"
assertValBool :: Value -> Boolean
assertValBool = case _ of
    BoolVal x -> x
    _ -> bug "assertValint failed"
assertValList :: Value -> (List Value)
assertValList = case _ of
    ListVal x -> x
    _ -> bug "assertValint failed"
assertValFun :: Value -> (Value -> Either Error Value)
assertValFun = case _ of
    FunVal x -> x
    _ -> bug "assertValint failed"

data Error = HoleError | BoundaryError | FreeVarError

eval :: (List (Lazy (Either Error Value))) -> DerivTerm -> Either Error Value
eval env ((Grammar.DerivLabel r _) % kids) =
    case r /\ kids of
          Zero /\ [] -> force $ Util.fromJust' "eval Zero case" $ List.head env
          Suc /\ [x] -> eval (Util.fromJust' "eval suc" (List.tail env)) x
          Lam /\ [_name, _ty, t] -> pure $ FunVal (\x -> eval (pure (Right x) : env) t)
          Let /\ [_name, _ty, def, body] -> do
            let vDef = eval ((defer \_ -> vDef) : env) def
            eval (pure vDef : env) body
          App /\ [t1, t2] -> do
            v1 <- eval env t1
            v2 <- eval env t2
            assertValFun v1 v2
          GreyApp /\ [t, _] -> eval env t
          Var /\ [x] -> eval env x
          FreeVar /\ [] -> Left FreeVarError
          TermHole /\ [_type] -> Left HoleError
--          TypeHole /\ [] -> ?h
--          DataTypeRule dataType /\ [] -> ?h
--          ArrowRule /\ [] -> ?h
--          ListRule /\ [] -> ?h
          Newline /\ [t] -> eval env t
          If /\ [cond, thenn, elsee] -> do
            vCond <- eval env cond
--            vThenn <- eval env thenn
--            vElsee <- eval env elsee
            if (assertValBool vCond) then eval env thenn else eval env elsee
          ErrorBoundary /\ [_] -> Left BoundaryError
          ConstantRule constant /\ [] -> pure $ evalConst constant
          InfixRule infixOperator /\ [t1, t2] -> do
            v1 <- eval env t1
            v2 <- eval env t2
            pure $ evalInfix infixOperator v1 v2
          EqualsRule /\ [t1, t2] -> do
            v1 <- eval env t1
            v2 <- eval env t2
            pure $ BoolVal (eqValue v1 v2)
          NilRule /\ [] -> pure $ ListVal Nil
          ConsRule /\ [] -> pure $ FunVal (\x -> pure (FunVal (\xs -> pure (ListVal (x : assertValList xs)))))
          LengthRule /\ [] -> pure $ FunVal (\xs -> pure (IntVal (List.length (assertValList xs))))
          HeadRule /\ [] -> pure $ FunVal (\xs -> pure (Util.fromJust (List.head (assertValList xs))))
          TailRule /\ [] -> pure $ FunVal (\xs -> pure (ListVal (Util.fromJust (List.tail (assertValList xs)))))
          IndexRule /\ [] -> pure $ FunVal (\xs -> pure (FunVal (\n -> pure (Util.fromJust (List.index (assertValList xs) (assertValInt n))))))
          ListMatchRule /\ [li, nilCase, _, _, consCase] -> do
            vLi <- eval env li
            case assertValList vLi of
                Nil -> eval env nilCase
                v : vs -> eval (pure (Right v) : pure (Right (ListVal vs)) : env) consCase
          IntegerLiteral /\ [Grammar.DerivLiteral (Grammar.DataInt n) % []] -> pure (IntVal n)
          Comment /\ [_, a] -> eval env a
          _ -> bug ("eval case fail: rule was " <> show r)
eval _ _ = bug "eval case shouldn't happen"

evalConst :: Constant -> Value
evalConst = case _ of
    ConstTrue -> BoolVal true
    ConstFalse -> BoolVal false
    ConstNot -> FunVal (\b -> pure (BoolVal (not (assertValBool b))))

evalInfix :: InfixOperator -> (Value -> Value -> Value)
evalInfix = case _ of
    OpPlus -> \x y -> IntVal (assertValInt x + assertValInt y)
    OpMinus -> \x y -> IntVal (assertValInt x - assertValInt y)
    OpTimes -> \x y -> IntVal (assertValInt x * assertValInt y)
    OpDivide -> \x y -> IntVal (assertValInt x / assertValInt y)
    OpMod -> \x y -> IntVal (mod (assertValInt x) (assertValInt y))
    OpPow -> \x y -> IntVal (pow (assertValInt x) (assertValInt y))
    OpLess -> \x y -> BoolVal (assertValInt x < assertValInt y)
    OpGreater -> \x y -> BoolVal (assertValInt x > assertValInt y)
    OpLessEq -> \x y -> BoolVal (assertValInt x <= assertValInt y)
    OpGreaterEq -> \x y -> BoolVal (assertValInt x >= assertValInt y)
    OpAnd -> \x y -> BoolVal (assertValBool x && assertValBool y)
    OpOr -> \x y -> BoolVal (assertValBool x || assertValBool y)

printValue :: Value -> String
printValue val = case val of
    BoolVal x -> show x
    IntVal x -> show x
    ListVal x -> List.foldr (\x xs -> "(cons " <> (printValue x) <> " " <> xs <> ")") "nil" x
    FunVal _ -> "<function>"

interpereter :: Grammar.DerivTerm PreSortLabel RuleLabel -> String
interpereter dterm = case eval Nil dterm of
    Left error -> case error of
        HoleError -> "Error: hole"
        BoundaryError -> "Error: type boundary"
        FreeVarError -> "Error: unbound variable"
    Right res -> printValue res
