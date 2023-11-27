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
import Data.Zippable as Zippable
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
import Type.Direction as Dir
import Data.Maybe as Maybe
import Data.Tuple (uncurry)

-- Makes edits which around any holes in the given term
makeWrapEdits :: forall l r. Grammar.IsRuleLabel l r =>
    (Sort l -> Boolean)
    -> ({bottom :: Sort l, top :: Sort l} -> Boolean)
    -> (DerivLabel l r -> Maybe (DerivLabel l r))
    -> Base.SplitChangeType l
    -> String
    -> Grammar.Sort l -> Grammar.DerivTerm l r -> List.List (Edit.Edit l r)
makeWrapEdits isValidCursorSort isValidSelectionSorts forgetSorts splitChange name cursorSort dterm =
--    let getPaths dzipper =
--            case Base.moveHDZUntil Dir.nextDir (\hdz -> Base.isValidCursor isValidCursorSort hdz && Base.hdzIsHolePosition hdz) (Base.HoleyDerivZipper dzipper false) of
--                Nothing -> List.Nil
--                Just (Base.HoleyDerivZipper zipper _) -> zipper List.: (getPaths zipper)
--    in
    let getPaths dzipper@(Expr.Zipper path term) =
            let rest = List.concat $ map getPaths (List.fromFoldable $ Zippable.zipDowns dzipper) in
            if isValidCursorSort (derivTermSort term) && Grammar.isHole (Expr.exprLabel term)
                then dzipper List.: rest
                else rest
    in
    flip List.mapMaybe (getPaths (Expr.Zipper (Expr.Path List.Nil) dterm)) \(Expr.Zipper path inside) ->
        do
        _ <- case path of -- cancel if the path is empty
                Expr.Path List.Nil -> Nothing
                _ -> Maybe.Just unit
        _ <- if isValidSelectionSorts {bottom: Grammar.derivTermSort inside, top: nonemptyUpPathTopSort path}
                    then Just unit else Nothing
        makeEditFromPath forgetSorts splitChange (path /\ (Grammar.nonemptyPathInnerSort path))
            name cursorSort

-- Makes an edit that inserts a path, and propagates the context change downwards and type change upwards
makeEditFromPath :: forall l r. Grammar.IsRuleLabel l r => (DerivLabel l r -> Maybe (DerivLabel l r)) -> Base.SplitChangeType l
    -> Grammar.DerivPath Up l r /\ Grammar.Sort l -> String -> Grammar.Sort l -> Maybe (Edit.Edit l r)
makeEditFromPath forgetSorts splitChange (path /\ bottomOfPathSort) name cursorSort = do
    action <- makeActionFromPath false forgetSorts splitChange path name cursorSort
    pure $ { label : name
    , action : defer \_ -> action -- TODO: Maybe I should find a way to use Lazy correctly here? And only the the necessary computation before it?
    }

makeActionFromPath :: forall l r. Grammar.IsRuleLabel l r =>
    Boolean -> (DerivLabel l r -> Maybe (DerivLabel l r)) -> Base.SplitChangeType l
    -> Grammar.DerivPath Up l r -> String -> Grammar.Sort l -> Maybe (Edit.Action l r)
makeActionFromPath cursorGoesInside forgetSorts splitChange path name cursorSort = do
    let change = Smallstep.getPathChange2 path forgetSorts
    let {upChange: preTopChange, cursorSort: preCursorSort, downChange: preBotChange} = splitChange change
    _ /\ sub <- unify preCursorSort cursorSort
    let topChange = ChangeAlgebra.subSomeMetaChange sub preTopChange
    let botChange = ChangeAlgebra.subSomeMetaChange sub (ChangeAlgebra.invert preBotChange)
    let pathSubbed = subDerivPath sub path
    pure $ Edit.WrapAction {
        topChange
        , dpath : pathSubbed -- DerivPath Up l r
        , botChange
        , sub
        , cursorGoesInside
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
