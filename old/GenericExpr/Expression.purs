module Language.Pantograph.GenericExpr.Expression where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Data.List (List)
import Data.List as List
import Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import Data.Either (Either(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Data.UUID (UUID)

--type WrappedChild ExprLabel wrap = wrap /\ (Expr ExprLabel wrap)
-- TODO: Do we ever need Expr or just ExprWithMetavars?

data Expr ExprLabel = Expr ExprLabel (List (Expr ExprLabel))

data ExprWMExprLabel ExprLabel = ExprWM ExprLabel | EMetaVar UUID
type ExprWM ExprLabel = Expr (ExprWMExprLabel ExprLabel)

data ToothExprLabel ExprLabel = Tooth ExprLabel {-List (Expr ExprLabel)-} {-List (Expr ExprLabel)-}
type Tooth ExprLabel = Expr (Either (ToothExprLabel ExprLabel) ExprLabel)

data ListExprLabel ExprLabel = ConsExprLabel {-x-} {-xs-} | NilExprLabel
type Path ExprLabel = Expr (Either (ListExprLabel ExprLabel) (Either (ToothExprLabel ExprLabel) ExprLabel))

-- A Change is just an expression with a few extra possible ExprLabels: namely, Replace, Plus, and Minus.
type GChange ExprLabel = ExprWM (ChangeExprLabel ExprLabel)

data ChangeExprLabel ExprLabel
    = ChangeExpr ExprLabel {-whatever kids that ExprLabel had-}
    | Plus ExprLabel Int {- has whatever kids that ExprLabel had except one, and the Int tells which one -}
    | Minus ExprLabel Int {- same as Plus -}
    | Replace {-Expr ExprLabel-} {-Expr ExprLabel-}


--data TypingRule ExprLabel = TypingRule
--    (ExprWM ExprLabel) -- The sort of the expression overall
--    (List (ExprWM ExprLabel)) -- The sort of each child

--data

--- below this line is garbage

-- TypingRuleExprLabel
data AnnotatedExprLabel ExprLabel = OfSort {-sort-} {-term-} | ALOther ExprLabel
type Annotated ExprLabel = ExprWM (AnnotatedExprLabel ExprLabel)

shouldntBeAnnotations :: forall ExprLabel. AnnotatedExprLabel ExprLabel -> ExprLabel
shouldntBeAnnotations (ALOther l) = l
shouldntBeAnnotations _ = unsafeThrow "assumption violated: there was an annotation"

--data TypingRulesExprLabel ExprLabel = TypingRule {-parent sort-} {-list of children sorts-} | TRCons {-sort-} {-sorts-} | TRNil | TROther ExprLabel
--type TypingRule ExprLabel = Expr (TypingRulesExprLabel ExprLabel)

-- TODO: design decision: should these be working with annotated terms?
data TypingRule ExprLabel =
    TypingRule
    (ExprWM ExprLabel) -- The parent's sort
    (List (ExprWM ExprLabel)) -- The children node's sorts

type Language ExprLabel = ExprLabel -> TypingRule ExprLabel

data MapChange ExprLabel = MCPlus (Expr ExprLabel) | MCMinus (Expr ExprLabel) | MCChange (GChange ExprLabel)

--------------------------------------------------------------------------------

-- Typechange injection is just being a functor!
instance Functor Expr where
    map :: forall ExprLabel1 ExprLabel2 . (ExprLabel1 -> ExprLabel2) -> Expr ExprLabel1 -> Expr ExprLabel2
    map f (Expr ExprLabel kids) = Expr (f ExprLabel) (map (map f) kids)

derive instance eqAnnotatedExprLabel :: Eq ExprLabel => Eq (AnnotatedExprLabel ExprLabel)
derive instance eqExprWMExprLabel :: Eq ExprLabel => Eq (ExprWMExprLabel ExprLabel)
--derive instance eqTypingRulesExprLabel :: Eq ExprLabel => Eq (TypingRulesExprLabel ExprLabel)

derive instance functorExprWMExprLabel :: Functor ExprWMExprLabel
derive instance functorExprAnnotatedExprLabel :: Functor AnnotatedExprLabel
--derive instance functorTypingRuleExprLabel :: Functor TypingRulesExprLabel

--data GTypingRuleEntry ExprLabel id = TypingRuleEntry (Map id (MapChange ExprLabel)) (GChange ExprLabel)
--data GTypingRule ExprLabel id = TypingRule (List (GTypingRuleEntry ExprLabel id))

--{-
--While this isn't dependent type theory so we can't ensure that Exprs, GChanges etc. satisfy typing rules
--intrinsically, we can write checking functions:
---}
--exprIsTyped :: forall ExprLabel wrap id .
--    List (GTypingRule ExprLabel id) -- The typing rules
--    -> Expr ExprLabel -- The sort (which contains the type)
--    -> Map id (Expr ExprLabel) -- The context - a mapping from ids to sorts
--    -> Expr ExprLabel -- The expression to be type-checked
--    -> Boolean
--exprIsTyped = unsafeThrow "todo"
--
--instance Eq ExprLabel => Eq (Expr ExprLabel) where
--    eq (Expr l1 kids1) (Expr l2 kids2) = l1 == l2 && (List.all identity (eq <$> kids1 <*> kids2))
--    eq (EMetaVar x) (EMetaVar y) = x == y
--    eq _ _ = false
--
--inject :: forall a . Expr a -> GChange a
--inject (Expr l kids) = ChangeExpr l (map inject kids)
--inject (EMetaVar x) = MetaVar x
