module Language.Pantograph.Expression where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.List (List)
import Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import Data.Either (Either(..))

type WrappedChild label wrap = wrap /\ (Expr label wrap)
data Expr label wrap = Expr label (Array (WrappedChild label wrap))

data Tooth label wrap = Tooth label (Array (WrappedChild label wrap)) (Array (WrappedChild label wrap))

type Path label wrap = List (wrap /\ Tooth label wrap)

-- Changes, generically over any grammar!
data GChange label = ChangeExpr label (Array (GChange label))
    | Plus label (Array (GChange label)) (GChange label) (Array (GChange label))
    | Minus label (Array (GChange label))  (GChange label) (Array (GChange label))
    | Replace (ExprWithMetavars label) (ExprWithMetavars label)
    | MetaVar Int {-figure out UUID or something - this is supposed to be a metavariable -}

data ExprWithMetavars label = ExprWM label (Array (ExprWithMetavars label)) | EMetaVar Int

data MapChange label = MCPlus (ExprWithMetavars label) | MCMinus (ExprWithMetavars label) | MCChange (GChange label)

data GTypingRuleEntry label id = TypingRuleEntry (Map id (MapChange label)) (GChange label)
data GTypingRule label id = TypingRule (Array (GTypingRuleEntry label id))


{-
Using this generic view of an expression, we can define functions which map over expressions dealing with variables
and binders generically.

-- The question "what is a variable" is dependent on the context - sometimes term variables, sometimes holes, etc
-- Same with the question "what is a binder"

So we need our notions of generic mapping to respect this
-}


{-
Generic function map over expressions.
At each node, the function can choose if it should pass through to the child nodes, or if it should
replace that node with something else.
-}
type LabelMap l w env = (env -> l -> Array (WrappedChild l w) -> Either (Expr l w) (Array env))

-- PROBLEM: LabelMap doesn't really work going UP paths!
exprMap :: forall l w env . LabelMap l w env -> env -> Expr l w -> Expr l w
exprMap f env (Expr label children) =
    case f env label children of
        Left expr' -> expr'
        Right childEnvs ->
            let children' = map (\(env' /\ wrap /\ child) -> wrap /\ exprMap f env' child) (Array.zip childEnvs children) in
            Expr label children'

data LabelInfo id = IsVar id | Binds (Set id) -- for each child that is bound, update the environment

expMapFreeVars :: forall l w id . Ord id => (l -> LabelInfo id) -> (id -> Expr l w) -> Expr l w -> Expr l w
expMapFreeVars labelInfo atVar expr =
    let mapper bound label children = case labelInfo label of
            IsVar id -> if Set.member id bound
                then (Right (Array.replicate (Array.length children) bound))
                else (Left (atVar id))
            Binds newBinds ->
                let newBound = Set.union newBinds bound in
                (Right (Array.replicate (Array.length children) newBound))
    in exprMap mapper Set.empty expr
