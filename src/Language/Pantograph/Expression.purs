module Language.Pantograph.Expression where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.List (List)
import Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import Data.Either (Either(..))
import Effect.Exception.Unsafe (unsafeThrow)

type WrappedChild label wrap = wrap /\ (Expr label wrap)
data Expr label wrap = Expr label (Array (WrappedChild label wrap))

data Tooth label wrap = Tooth label (Array (WrappedChild label wrap)) (Array (WrappedChild label wrap))

type Path label wrap = List (wrap /\ Tooth label wrap)

-- Changes, generically over any grammar!
data GChange label = ChangeExpr label (Array (GChange label))
    | Plus label (Array (ExprWithMetavars label)) (GChange label) (Array (ExprWithMetavars label))
    | Minus label (Array (ExprWithMetavars label))  (GChange label) (Array (ExprWithMetavars label))
    | Replace (ExprWithMetavars label) (ExprWithMetavars label)
    | MetaVar Int {-figure out UUID or something - this is supposed to be a metavariable -}

data ExprWithMetavars label = ExprWM label (Array (ExprWithMetavars label)) | EMetaVar Int

data MapChange label = MCPlus (ExprWithMetavars label) | MCMinus (ExprWithMetavars label) | MCChange (GChange label)

data GTypingRuleEntry label id = TypingRuleEntry (Map id (MapChange label)) (GChange label)
data GTypingRule label id = TypingRule (Array (GTypingRuleEntry label id))

instance Eq label => Eq (ExprWithMetavars label) where
    eq (ExprWM l1 kids1) (ExprWM l2 kids2) = l1 == l2 && (Array.all identity (eq <$> kids1 <*> kids2))
    eq (EMetaVar x) (EMetaVar y) = x == y
    eq _ _ = false

inject :: forall a . ExprWithMetavars a -> GChange a
inject (ExprWM l kids) = ChangeExpr l (map inject kids)
inject (EMetaVar x) = MetaVar x

composeChange :: forall label. Eq label => GChange label -> GChange label -> GChange label
composeChange (ChangeExpr l1 kids1) (ChangeExpr l2 kids2) =
    if not (l1 == l2) then unsafeThrow "shouldn't happen: these changes don't line up" else
    if not (Array.length kids1 == Array.length kids2) then unsafeThrow "shouldn't happen: should have same number of kids" else
    ChangeExpr l1 (composeChange <$> kids1 <*> kids2)
composeChange (MetaVar x) (MetaVar y) | x == y = MetaVar x
composeChange (Minus l1 leftKids1 child1 rightKids1) (Plus l2 leftKids2 child2 rightKids2)
    | l1 == l2
      && leftKids1 == leftKids2
      && Array.length rightKids1 == Array.length rightKids2
      = ChangeExpr l1 ((map inject leftKids1) <> [composeChange child1 child2] <> (map inject rightKids1))
composeChange (Plus l1 leftKids1 child1 rightKids1) (Minus l2 leftKids2 child2 rightKids2)
    | l1 == l2
      && leftKids1 == leftKids2
      && Array.length rightKids1 == Array.length rightKids2
      = composeChange child1 child2
composeChange _ _ = unsafeThrow "TODO: not yet defined"
--composeChange a (Plus tooth b) = Plus tooth (composeChange a b)
--composeChange (Minus tooth a) b = Minus tooth (composeChange a b)
--composeChange (Plus t1 a) (Minus t2 b) | t1 == t2 = composeChange a b
--composeChange (Plus t a) (CArrow c b) =
--    if not (tyInject t == c) then unsafeThrow "shouldn't happen in composeChange 1" else
--    Plus t (composeChange a b)
--composeChange (CArrow c a) (Minus t b) =
--    if not (tyInject t == c) then unsafeThrow "shouldn't happen in composeChange 2" else
--    Minus t (composeChange a b)
--composeChange (CNeu x1 args1) (CNeu x2 args2) | x1 == x2 =
--    CNeu x1 (composeParamChanges args1 args2)
---- TODO: It should be possible to compose changes more generally. Come back to this!
--composeChange c1 c2 =
--    let a /\ b = getEndpoints c1 in
--    let b' /\ c = getEndpoints c2 in
--    if b == b' then Replace a c else
--        unsafeThrow ("composeChange is only valid to call on changes which share a type endpoint. c1 is " <> show c1 <> "and c2 is " <> show c2)


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
