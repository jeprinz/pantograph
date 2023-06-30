-- | # Argon -- a scoped, untyped language with only neutral applications.
-- |
-- | <<Term>> : Term(<<Name>> , ... , [])
-- |
-- | <<Label>> : Label(<<String>>)
-- |
-- | <<Args>> : Args(<<Term>> , ... , [])
-- |
-- | <<Sort>> ::= Name
-- |            | Term
-- |            | <<Sort>> , <<Sort>> | []
-- |            | Label(<<String>>)
-- | 
-- | <<Term>> ::= <<Name>>(<<Args>>) 
-- |            | fun (<<Name>>) => <<Term>> 
-- |            | let <<Name>> = <<Term>> in <<Term>>
-- | 
-- | <<Args>> ::= <<term>> , <<args>> | []
-- |
-- | <<Name> ::= Name(<<Label>>)
-- |
module Language.Pantograph.Argon where

import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Bug.Assertion (assert, assertI, just)
import Control.Plus (empty)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel, slot, (%), (%$), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Lazy (defer)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Variant (Variant)
import Debug (traceM, trace)
import Debug as Debug
import Effect.Exception.Unsafe (unsafeThrow)
import Halogen.HTML as HH
import Halogen.Utilities (classNames)
import Hole (hole)
import Language.Pantograph.Generic.ChangeAlgebra (rEndpoint)
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Language.Pantograph.Generic.Edit (newPathFromRule)
import Language.Pantograph.Generic.Edit as Edit
import Language.Pantograph.Generic.Grammar (defaultDerivTerm', isHoleRuleTotalMap, prettyExprF'_unsafe_RuleLabel, sor, (%|-), (%|-*))
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base (EditorSpec)
import Language.Pantograph.Generic.Rendering.Base as Rendering
import Language.Pantograph.Generic.Rendering.Console (logConsole)
import Language.Pantograph.Generic.Rendering.Elements as Rendering
import Language.Pantograph.Generic.Smallstep ((%+-), dPLUS, dMINUS)
import Language.Pantograph.Generic.Smallstep (StepExprLabel(..), cSlot, dTERM, dMTERM)
import Language.Pantograph.Generic.Smallstep as SmallStep
import Language.Pantograph.Generic.Unification (unify)
import Text.Pretty (class Pretty, parens, pretty, (<+>))
import Text.Pretty as P
import Type.Direction (Up)
import Util (fromJust)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Expr = Expr.Expr PreSortLabel
type MetaExpr = Expr.MetaExpr PreSortLabel
type Zipper = Expr.Zipper PreSortLabel
type Tooth = Expr.Tooth PreSortLabel
type Sort = Grammar.Sort PreSortLabel
type Language = Grammar.Language PreSortLabel RuleLabel
type Rule = Grammar.Rule PreSortLabel

--------------------------------------------------------------------------------
-- PreSortLabel
--------------------------------------------------------------------------------

data PreSortLabel
  = VarSort
  | TermSort
  | ArgsSort

derive instance Generic PreSortLabel _
instance Show PreSortLabel where show x = genericShow x
instance Eq PreSortLabel where eq x = genericEq x
instance Ord PreSortLabel where compare x y = genericCompare x y
instance Pretty PreSortLabel where pretty x = genericShow x

instance IsExprLabel PreSortLabel where
  prettyExprF'_unsafe = hole "TOOD"

  expectedKidsCount = hole "TODO"

--------------------------------------------------------------------------------
-- RuleLabel
--------------------------------------------------------------------------------

data RuleLabel
  -- <<Term>>
  = Var
  | Neu
  | Lam
  | Let
  | TermHole
  -- | <<Args>>
  | ConsArgs
  | NilArgs

derive instance Generic RuleLabel _
derive instance Eq RuleLabel
derive instance Ord RuleLabel
instance Show RuleLabel where show x = genericShow x
instance Enum RuleLabel where
  pred x = genericPred x
  succ x = genericSucc x
instance Bounded RuleLabel where
  bottom = genericBottom
  top = genericTop
instance Pretty RuleLabel where pretty = show

instance Grammar.IsRuleLabel PreSortLabel RuleLabel where
  -- <<Term>>
  prettyExprF'_unsafe_RuleLabel (Var /\ [label]) = pretty label
  prettyExprF'_unsafe_RuleLabel (Neu /\ [apl, args]) = pretty apl <> parens (pretty args)
  prettyExprF'_unsafe_RuleLabel (Lam /\ [name, bod]) = "(fun " <> pretty name <> " => " <> pretty bod <> ")"
  prettyExprF'_unsafe_RuleLabel (Let /\ [name, imp, bod]) = "(let " <> pretty imp <> " = " <> pretty name <> " in " <> pretty bod <> ")"
  -- <<Args>>
  prettyExprF'_unsafe_RuleLabel (ConsArgs /\ [term, args]) = pretty term <> ", " <> pretty args
  prettyExprF'_unsafe_RuleLabel (NilArgs /\ []) = ""
    
  language = language
  
  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
    TermHole -> true
    _ -> false
  
  defaultDerivTerm' 
    (Expr.Meta (Right (Grammar.InjectSortLabel TermSort)) % []) =
    Just $ Grammar.makeLabel TermHole [] [] % []

  defaultDerivTerm'
    (Expr.Meta (Right (Grammar.InjectSortLabel ArgsSort)) % []) =
    Just $ Grammar.makeLabel NilArgs [] [] % []

  defaultDerivTerm' 
    sort =
    bug $ "[defaultDerivTerm'] no match: " <> show sort
  
--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

language :: Language
language = TotalMap.makeTotalMap case _ of
  -- <<Term>>
  Var -> Grammar.makeRule [] ["x"] \[x] ->
    [ Grammar.NameSortLabel %* [x] ] /\
    ( TermSort %|-* [] )
  Neu -> Grammar.makeRule [] [] \[x] ->
    [ VarSort %|-* [x] ] /\
    ( ArgsSort %|-* [] )
  Lam -> Grammar.makeRule [] [] \[] ->
    [ VarSort %|-* [] ] /\
    ( TermSort %|-* [] )
  Let -> Grammar.makeRule [] [] \[] ->
    [ VarSort %|-* []
    , TermSort %|-* []
    , TermSort %|-* [] ] /\
    ( TermSort %|-* [] )
  TermHole -> Grammar.makeRule [] [] \[] ->
    [ TermSort %|-* [] ] /\
    ( TermSort %|-* [] )
  -- <<Args>>
  ConsArgs -> Grammar.makeRule [] [] \[] ->
    [ TermSort %|-* []
    , ArgsSort %|-* [] ] /\
    ( ArgsSort %|-* [] )
  NilArgs -> Grammar.makeRule [] [] \[] ->
    [ ArgsSort %|-* [] ] /\
    ( ArgsSort %|-* [] )
