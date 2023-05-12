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

--type WrappedChild ExprLabel wrap = wrap /\ (Expr ExprLabel wrap)
-- TODO: Do we ever need Expr or just ExprWithMetavars?
data Expr ExprLabel = Expr ExprLabel (Array (Expr ExprLabel))

data Tooth ExprLabel = Tooth ExprLabel (Array (Expr ExprLabel)) (Array (Expr ExprLabel))

type Path ExprLabel = List (Tooth ExprLabel)

-- Changes, generically over any grammar!
data GChange ExprLabel = ChangeExpr ExprLabel (Array (GChange ExprLabel))
    | Plus ExprLabel (Array (ExprWithMetavars ExprLabel)) (GChange ExprLabel) (Array (ExprWithMetavars ExprLabel))
    | Minus ExprLabel (Array (ExprWithMetavars ExprLabel))  (GChange ExprLabel) (Array (ExprWithMetavars ExprLabel))
    | Replace (ExprWithMetavars ExprLabel) (ExprWithMetavars ExprLabel)
    | MetaVar Int {-figure out UUID or something - this is supposed to be a metavariable -}

data ExprWithMetavars ExprLabel = ExprWM ExprLabel (Array (ExprWithMetavars ExprLabel)) | EMetaVar Int

data MapChange ExprLabel = MCPlus (ExprWithMetavars ExprLabel) | MCMinus (ExprWithMetavars ExprLabel) | MCChange (GChange ExprLabel)

data GTypingRuleEntry ExprLabel id = TypingRuleEntry (Map id (MapChange ExprLabel)) (GChange ExprLabel)
data GTypingRule ExprLabel id = TypingRule (Array (GTypingRuleEntry ExprLabel id))

{-
While this isn't dependent type theory so we can't ensure that Exprs, GChanges etc. satisfy typing rules
intrinsically, we can write checking functions:
-}
exprIsTyped :: forall ExprLabel wrap id .
    Array (GTypingRule ExprLabel id) -- The typing rules
    -> Expr ExprLabel -- The sort (which contains the type)
    -> Map id (Expr ExprLabel) -- The context - a mapping from ids to sorts
    -> Expr ExprLabel -- The expression to be type-checked
    -> Boolean
exprIsTyped = unsafeThrow "todo"

instance Eq ExprLabel => Eq (ExprWithMetavars ExprLabel) where
    eq (ExprWM l1 kids1) (ExprWM l2 kids2) = l1 == l2 && (Array.all identity (eq <$> kids1 <*> kids2))
    eq (EMetaVar x) (EMetaVar y) = x == y
    eq _ _ = false

inject :: forall a . ExprWithMetavars a -> GChange a
inject (ExprWM l kids) = ChangeExpr l (map inject kids)
inject (EMetaVar x) = MetaVar x

composeChange :: forall ExprLabel. Eq ExprLabel => GChange ExprLabel -> GChange ExprLabel -> GChange ExprLabel
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
type ExprLabelMap l env = (env -> l -> Array (Expr l) -> Either (Expr l) (Array env))

-- PROBLEM: ExprLabelMap doesn't really work going UP paths!
exprMap :: forall l env . ExprLabelMap l env -> env -> Expr l -> Expr l
exprMap f env (Expr ExprLabel children) =
    case f env ExprLabel children of
        Left expr' -> expr'
        Right childEnvs ->
            let children' = map (\(env' /\ child) -> exprMap f env' child) (Array.zip childEnvs children) in
            Expr ExprLabel children'

data ExprLabelInfo id = IsVar id | Binds (Set id) -- for each child that is bound, update the environment

expMapFreeVars :: forall l id . Ord id => (l -> ExprLabelInfo id) -> (id -> Expr l) -> Expr l -> Expr l
expMapFreeVars ExprLabelInfo atVar expr =
    let mapper bound ExprLabel children = case ExprLabelInfo ExprLabel of
            IsVar id -> if Set.member id bound
                then (Right (Array.replicate (Array.length children) bound))
                else (Left (atVar id))
            Binds newBinds ->
                let newBound = Set.union newBinds bound in
                (Right (Array.replicate (Array.length children) newBound))
    in exprMap mapper Set.empty expr
