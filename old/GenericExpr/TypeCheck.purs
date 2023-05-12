module Language.Pantograph.GenericExpr.TypeCheck where

import Prelude
import Language.Pantograph.GenericExpr.Expression
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Language.Pantograph.GenericExpr.Unification
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception.Unsafe (unsafeThrow)
import Util (union')

typecheck :: forall l. Eq l => Language l
--    -> Expr l {-the sort-}
    -> Annotated l {-the expression-}
    -> Boolean
typecheck lang {-sort-} expr =
    let go :: Annotated l -> Maybe (Sub l)
        go expr =
            case expr of
                Expr (ExprWM OfSort)
                    (sort -- (Expr (ExprWM (ALOther s)) skids)
                    : (Expr (ExprWM (ALOther ExprLabel)) eKids)
                    : Nil) ->
                    case lang ExprLabel of
                        TypingRule parentSort kidSorts ->
                            do
    --                            (s' /\ sub1) <- unify parentSort (ExprWM <$> sort)
                                (s' /\ sub1) <- unify ((map ALOther) <$> parentSort) sort
                                let unifyEKids :: List (Annotated l) -> List (ExprWM l) -> Maybe (Sub l)
                                    unifyEKids Nil Nil = Just Map.empty
                                    unifyEKids (kid@(Expr (ExprWM OfSort) (actualSort : _kidd : Nil)) : eKids) (expectedSort : kidSorts) = do
                                        _ <- go kid -- Typecheck the kid itself
                                        _ /\ sub1' <- unify ((map ALOther) <$> expectedSort) actualSort
                                        sub2 <- unifyEKids (map (subExpr sub1') eKids) (map (subExpr ((map (map (map shouldntBeAnnotations))) sub1')) kidSorts)
                                        pure $ union' ((map (map (map shouldntBeAnnotations))) sub1') sub2
                                    unifyEKids _ _ = unsafeThrow "lengths of lists were different"
                                sub2 <- unifyEKids eKids kidSorts
                                pure $ union' ((map (map (map shouldntBeAnnotations))) sub1) sub2
                _ -> unsafeThrow "annotated expression not well formed"
    in case go expr of
       Just _ -> true
       Nothing -> false
--data TRExprLabel ExprLabel = OfSort ExprLabel {-sort-} {-term-} | Other ExprLabel

{-

Problem with the `sort is part of expr` idea: how do I know what the children are so that I can check them as well?
How do I carry through knowledge that I got from unification on the parent?


A possible solution:
1) the sort isn't (directly) part of the expr
2) define:
data ExprWithSortExprLabel ExprLabel = ExprWithSortExprLabel ExprLabel | OfSort
type ExprWithSort ExprLabel = Expr (ExprWithSortExprLabel ExprLabel)

The idea is that here, a lambda can be represented by:

(Expr OfSort [Term (A -> B), (Expr Lambda [OfSort [VarName, name], OfSort [Term A, body]])]

3) typecheck requires it's input to be of the form (Expr OfSort [sort, (Expr ExprLabel [... kids ...])])
where it can then use `ExprLabel` to find the corresponding typing rule

QUESTION: Does this structure have to be an Expr, or can it just be something else?
Answer: yes it has to be an expr so it can be recursive correctly.

-}