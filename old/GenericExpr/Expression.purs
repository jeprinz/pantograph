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

--type WrappedChild label wrap = wrap /\ (Expr label wrap)
-- TODO: Do we ever need Expr or just ExprWithMetavars?

data Expr label = Expr label (List (Expr label))

data ExprWMLabel label = ExprWM label | EMetaVar UUID
type ExprWM label = Expr (ExprWMLabel label)

data ToothLabel label = Tooth label {-List (Expr label)-} {-List (Expr label)-}
type Tooth label = Expr (Either (ToothLabel label) label)

data ListLabel label = ConsLabel {-x-} {-xs-} | NilLabel
type Path label = Expr (Either (ListLabel label) (Either (ToothLabel label) label))

-- A Change is just an expression with a few extra possible labels: namely, Replace, Plus, and Minus.
type GChange label = ExprWM (ChangeLabel label)

data ChangeLabel label
    = ChangeExpr label {-whatever kids that label had-}
    | Plus label Int {- has whatever kids that label had except one, and the Int tells which one -}
    | Minus label Int {- same as Plus -}
    | Replace {-Expr label-} {-Expr label-}


--data TypingRule label = TypingRule
--    (ExprWM label) -- The sort of the expression overall
--    (List (ExprWM label)) -- The sort of each child

--data

--- below this line is garbage

-- TypingRuleLabel
data AnnotatedLabel label = OfSort {-sort-} {-term-} | ALOther label
type Annotated label = ExprWM (AnnotatedLabel label)

shouldntBeAnnotations :: forall label. AnnotatedLabel label -> label
shouldntBeAnnotations (ALOther l) = l
shouldntBeAnnotations _ = unsafeThrow "assumption violated: there was an annotation"

--data TypingRulesLabel label = TypingRule {-parent sort-} {-list of children sorts-} | TRCons {-sort-} {-sorts-} | TRNil | TROther label
--type TypingRule label = Expr (TypingRulesLabel label)

-- TODO: design decision: should these be working with annotated terms?
data TypingRule label =
    TypingRule
    (ExprWM label) -- The parent's sort
    (List (ExprWM label)) -- The children node's sorts

type Language label = label -> TypingRule label

data MapChange label = MCPlus (Expr label) | MCMinus (Expr label) | MCChange (GChange label)

--------------------------------------------------------------------------------

-- Typechange injection is just being a functor!
instance Functor Expr where
    map :: forall label1 label2 . (label1 -> label2) -> Expr label1 -> Expr label2
    map f (Expr label kids) = Expr (f label) (map (map f) kids)

derive instance eqAnnotatedLabel :: Eq label => Eq (AnnotatedLabel label)
derive instance eqExprWMLabel :: Eq label => Eq (ExprWMLabel label)
--derive instance eqTypingRulesLabel :: Eq label => Eq (TypingRulesLabel label)

derive instance functorExprWMLabel :: Functor ExprWMLabel
derive instance functorExprAnnotatedLabel :: Functor AnnotatedLabel
--derive instance functorTypingRuleLabel :: Functor TypingRulesLabel

--data GTypingRuleEntry label id = TypingRuleEntry (Map id (MapChange label)) (GChange label)
--data GTypingRule label id = TypingRule (List (GTypingRuleEntry label id))

--{-
--While this isn't dependent type theory so we can't ensure that Exprs, GChanges etc. satisfy typing rules
--intrinsically, we can write checking functions:
---}
--exprIsTyped :: forall label wrap id .
--    List (GTypingRule label id) -- The typing rules
--    -> Expr label -- The sort (which contains the type)
--    -> Map id (Expr label) -- The context - a mapping from ids to sorts
--    -> Expr label -- The expression to be type-checked
--    -> Boolean
--exprIsTyped = unsafeThrow "todo"
--
--instance Eq label => Eq (Expr label) where
--    eq (Expr l1 kids1) (Expr l2 kids2) = l1 == l2 && (List.all identity (eq <$> kids1 <*> kids2))
--    eq (EMetaVar x) (EMetaVar y) = x == y
--    eq _ _ = false
--
--inject :: forall a . Expr a -> GChange a
--inject (Expr l kids) = ChangeExpr l (map inject kids)
--inject (EMetaVar x) = MetaVar x
