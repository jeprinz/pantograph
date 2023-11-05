module Language.Pantograph.Lib.DefaultEdits where

import Prelude

import Language.Pantograph.Generic.Grammar
import Language.Pantograph.Generic.Smallstep
import Language.Pantograph.Generic.Unification
import Prelude

import Bug (bug)
import Bug.Assertion (assert, assertI, just)
import Control.Plus (empty)
import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Expr ((%),(%<))
import Data.Expr as Expr
import Data.Lazy (Lazy, defer)
import Data.List as List
import Data.List.Zip as ZipList
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.TotalMap as TotalMap
import Data.Traversable (sequence)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Hole (hole)
import Text.Pretty (pretty)
import Type.Direction (Up)
import Debug (traceM)
import Util (fromJust')
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Smallstep as Smallstep
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Language.Pantograph.Generic.Edit as Edit

-- Makes an edit that inserts a path, and propagates the context change downwards and type change upwards
makeEditFromPath :: forall l r. Grammar.IsRuleLabel l r => (DerivLabel l r -> Maybe (DerivLabel l r)) -> Base.SplitChangeType l
    -> Grammar.DerivPath Up l r /\ Grammar.Sort l -> String -> Grammar.Sort l -> Maybe (Edit.Edit l r)
makeEditFromPath forgetSorts splitChange (path /\ bottomOfPathSort) name cursorSort = do
    let change = Smallstep.getPathChange2 path forgetSorts
    let {upChange: preTopChange, cursorSort: preCursorSort, downChange: preBotChange} = splitChange change
    _ /\ sub <- unify preCursorSort cursorSort
    let topChange = ChangeAlgebra.subSomeMetaChange sub preTopChange
    let botChange = ChangeAlgebra.subSomeMetaChange sub (ChangeAlgebra.invert preBotChange)
    let pathSubbed = subDerivPath sub path
    pure $ { label : name
    , action : defer \_ -> Edit.WrapAction
    {
        topChange
        , dpath : pathSubbed -- DerivPath Up l r
        , botChange
        , sub
    }
    }

makeSubEditFromTerm :: forall l r. Grammar.IsRuleLabel l r => Grammar.DerivTerm l r -> String -> Grammar.Sort l -> Maybe (Edit.Edit l r)
makeSubEditFromTerm dterm name cursorSort = do
    _ /\ sub <- unify (Grammar.derivTermSort dterm) cursorSort
    pure $ { label : name
    , action : defer \_ -> Edit.FillAction
        {
            sub
            , dterm : Grammar.subDerivTerm sub dterm
        }
    }

makeChangeEditFromTerm :: forall l r. Grammar.IsRuleLabel l r => Grammar.DerivTerm l r -> String -> Grammar.Sort l -> Maybe (Edit.Edit l r)
makeChangeEditFromTerm dterm name cursorSort = do
    newCursorSort /\ sub <- unify (Grammar.derivTermSort dterm) cursorSort
    pure $ { label : name
    , action : defer \_ -> Edit.ReplaceAction
        {
            topChange: ChangeAlgebra.diff cursorSort newCursorSort
            , dterm : Grammar.subDerivTerm sub dterm
        }
    }
