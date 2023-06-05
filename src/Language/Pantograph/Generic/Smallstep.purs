module Language.Pantograph.Generic.Smallstep where

import Data.Foldable
import Language.Pantograph.Generic.ChangeAlgebra
import Language.Pantograph.Generic.Unification
import Prelude hiding (compose)

import Bug as Bug
import Bug.Assertion (Assertion(..), assert, makeAssertionBoolean)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel, (%), (%<))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Rev as Rev
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.TotalMap (TotalMap)
import Data.TotalMap as TotalMap
import Data.TotalMap as TotalMap
import Data.Tuple (snd, fst)
import Data.Tuple.Nested (type (/\), (/\))
import Hole (hole)
import Hole as Hole
import Language.Pantograph.Generic.ChangeAlgebra (endpoints)
import Language.Pantograph.Generic.Grammar (class IsRuleLabel, isHoleDerivLabel, isHoleDerivTerm)
import Language.Pantograph.Generic.Grammar as Grammar
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, braces, brackets, pretty)
import Type.Direction as Dir
import Util (lookup', fromJust')
import Utility ((<$$>))
import Debug (trace)

data Direction = Up | Down -- TODO:

derive instance Generic Direction _
instance Show Direction where show x = genericShow x
instance Eq Direction where eq x y = genericEq x y
instance Ord Direction where compare x y = genericCompare x y
instance Pretty Direction where
    pretty Up = "↑"
    pretty Down = "↓"

data StepExprLabel l r = Inject (Grammar.DerivLabel l r) | Cursor | Boundary Direction (Grammar.SortChange l) -- (Expr.MetaChange l)
type SSTerm l r = Expr.Expr (StepExprLabel l r)

type StepRule l r = SSTerm l r -> Maybe (SSTerm l r)

derive instance Generic (StepExprLabel l r) _
instance (Show l, Show r) => Show (StepExprLabel l r) where show x = genericShow x
instance (Eq l, Eq r) => Eq (StepExprLabel l r) where eq x y = genericEq x y
instance (Ord l, Ord r) => Ord (StepExprLabel l r) where compare x y = genericCompare x y
instance (IsExprLabel l, Pretty r) => Pretty (StepExprLabel l r) where
    pretty (Inject dl) = pretty dl
    pretty Cursor = "⌶"
    pretty (Boundary dir sortCh) = pretty dir <> brackets (pretty sortCh)

instance IsRuleLabel l r => Expr.IsExprLabel (StepExprLabel l r) where
    prettyExprF'_unsafe (Inject dl /\ kids) = Expr.prettyExprF'_unsafe (dl /\ kids)
    prettyExprF'_unsafe (sel@Cursor /\ [kid]) = pretty sel <> braces kid
    prettyExprF'_unsafe (sel@(Boundary _ _) /\ [kid]) = pretty sel <> braces kid
    
    expectedKidsCount (Inject dl) = Expr.expectedKidsCount dl
    expectedKidsCount Cursor = 1
    expectedKidsCount (Boundary _ _) = 1

-- ---------- Code for converting zippers to terms and back ------------------------------------------

addToothToTerm :: forall l r. Expr.Tooth (Grammar.DerivLabel l r) -> SSTerm l r -> SSTerm l r
addToothToTerm (Expr.Tooth l (ZipList.Path {left, right})) t =
 Expr.Expr (Inject l) $
     Array.fromFoldable (map (map Inject) (Rev.unreverse left)) <>
     [t] <>
     Array.fromFoldable (map (map Inject) right)

-- !TODO use Expr.Zipper
zipperToTerm :: forall l r. Expr.Path Dir.Up (Grammar.DerivLabel l r) -> Grammar.DerivTerm l r -> SSTerm l r
zipperToTerm (Expr.Path Nil) exp = Expr.Expr Cursor [map Inject exp]
zipperToTerm (Expr.Path (th : path)) exp = addToothToTerm th (zipperToTerm (Expr.Path path) exp)

termToSSTerm :: forall l r. Grammar.DerivTerm l r -> SSTerm l r
termToSSTerm = map Inject

wrapCursor :: forall l r. SSTerm l r -> SSTerm l r
wrapCursor t = Expr.Expr Cursor [t]

wrapPath :: forall l r. Grammar.DerivPath Dir.Up l r -> SSTerm l r -> SSTerm l r
wrapPath (Expr.Path Nil) t = t
wrapPath (Expr.Path (th : path)) t = addToothToTerm th (wrapPath (Expr.Path path) t)

setupSSTermFromWrapAction :: forall l r.
    Grammar.DerivPath Dir.Up l r -> -- top path
    Grammar.SortChange l -> -- change that goes between top path and inserted path 
    Grammar.DerivPath Dir.Up l r -> -- inserted path 
    Grammar.SortChange l -> -- change the goes between inserted path and bot path
    Grammar.DerivTerm l r -> -- bot term
    SSTerm l r
setupSSTermFromWrapAction topPath topCh insertedPath bottomCh botTerm =
    wrapPath topPath $ wrapBoundary Up topCh $ wrapPath insertedPath $ wrapCursor $ wrapBoundary Down bottomCh $ termToSSTerm botTerm

setupSSTermFromReplaceAction :: forall l r. 
    Grammar.DerivPath Dir.Up l r -> -- top path
    Grammar.SortChange l -> -- change that goes between top path and inserted path 
    Grammar.DerivTerm l r -> -- new term to replace with
    SSTerm l r
setupSSTermFromReplaceAction topPath ch newTerm =
    wrapPath topPath $ wrapBoundary Up ch $ wrapCursor $ termToSSTerm newTerm

assertJustExpr :: forall l r. SSTerm l r -> Grammar.DerivTerm l r
assertJustExpr (Expr.Expr (Inject l) kids) = Expr.Expr l (map assertJustExpr kids)
assertJustExpr _ = Bug.bug "Error: assertJustExpr assertion failed"

assertNoneOfList :: forall t b. List t -> (t -> Maybe b) -> List b
assertNoneOfList Nil _f = Nil
assertNoneOfList (x : xs) f = case f x of
 Nothing -> Bug.bug "assertNoneOfList: assertion failed"
 Just y -> y : assertNoneOfList xs f

-- assert that one or zero of the children will return a
oneOrNone :: forall t a b. List t -> (t -> Either a b) -> Either (List b) (List b /\ a /\ List b)
oneOrNone Nil _f = Left Nil
oneOrNone (x : xs) f = case f x of
 Right b ->
     case oneOrNone xs f of
         Right (bs1 /\ a /\ bs2) -> Right ((b : bs1) /\ a /\ bs2)
         Left bs -> Left (b : bs)
 Left a ->
     let bs2 = assertNoneOfList xs
             (\aa -> case f aa of
                     Left _ -> Nothing
                     Right b -> Just b)
     in
     Right (Nil /\ a /\ bs2)

termToZipper :: forall l r. SSTerm l r -> Expr.Zipper (Grammar.DerivLabel l r)
termToZipper (Expr.Expr Cursor [kid]) =
 Expr.Zipper (Expr.Path Nil) (assertJustExpr kid)
termToZipper (Expr.Expr (Inject l) kids) =
 let kids' = (\(Expr.Zipper p t) -> p /\ t) <$> (List.fromFoldable $ map termToZipper kids) in
 let isPath (p /\ e) = case p of
         (Expr.Path Nil) -> Right e
         _ -> Left (p /\ e) in
 let pathOrNot = oneOrNone kids' isPath in
 case pathOrNot of
     -- child didn't have cursor
     Left kids'' -> Expr.Zipper (Expr.Path Nil) (Expr.Expr l (Array.fromFoldable (kids'')))
     -- child has exactly one cursor
     Right (leftKids /\ (Expr.Path p /\ e) /\ rightKids) ->
        let newTooth = l %< ZipList.Path {left: Rev.reverse leftKids, right: rightKids} in
        Expr.Zipper (Expr.Path (newTooth : p)) e
--    let
termToZipper _ = Bug.bug "shouldn't happen"

--------------------------------------------------------------------------------

------------- Code for doing smallstep -------------------------------------

doAnyApply :: forall t out. t -> List (t -> Maybe out) -> Maybe out
doAnyApply t Nil = Nothing
doAnyApply t (r : rs) = case r t of
 Just t' -> Just t'
 Nothing -> doAnyApply t rs

stepSomebody :: forall l r. List (SSTerm l r) -> List (StepRule l r) -> Maybe (List (SSTerm l r))
stepSomebody Nil _ = Nothing
stepSomebody (t : ts) rules = case step t rules of
 Just t' -> Just (t' : ts)
 Nothing -> (:) <$> pure t <*> stepSomebody ts rules

-- when outputs `Nothing`, then done.
step :: forall l r. SSTerm l r -> List (StepRule l r) -> Maybe (SSTerm l r)
step t@(Expr.Expr l kids) rules =
 case doAnyApply t rules of
     Nothing -> do
         kids' <- stepSomebody (List.fromFoldable kids) rules
         pure $ Expr.Expr l (Array.fromFoldable kids')
     Just t' -> Just t'

stepRepeatedly :: forall l r. SSTerm l r -> List (StepRule l r) -> SSTerm l r
stepRepeatedly t rules = case step t rules of
    Nothing -> t
    Just t' -> stepRepeatedly t' rules

-------------- Default rules --------------------------------------------
--type Language l r = TotalMap r (Rule l)
--data Rule l = Rule (Set Expr.MetaVar) (Array (Sort l)) (Sort l)
--type Sort l = Expr.MetaExpr (SortLabel l)
--type SortChange l = Expr.MetaChange (SortLabel l)
--
--data SortLabel l
--  = InjectSortLabel l
--  | NameSortLabel
--  | StringSortLabel String

type SSChangeSort l = Expr.Expr (Expr.Meta (Expr.ChangeLabel (Expr.Meta (Grammar.SortLabel l))))
data SSChangeRule l = SSChangeRule (Set Expr.MetaVar) (Array (SSChangeSort l)) (SSChangeSort l)

-- The sorts in these are Expr (Meta (ChangeLabel (Meta l)).
-- the out Meta is the one that comes from the normal rules. The (ChangeLabel (Meta l))
-- corresponds with the changes in the boundaries, which are Expr (ChangeLabel (Meta l))
--type SSChLanguage l r = Grammar.Language (Expr.ChangeLabel (Expr.Meta l)) r
type SSChLanguage l r = TotalMap r (SSChangeRule l)

metaInject :: forall l. Expr.MetaExpr l -> Expr.MetaExpr (Expr.ChangeLabel (Expr.Meta l))
metaInject e = map (map (Expr.Inject <<< Expr.Meta <<< Right)) e

langToChLang :: forall l r. IsRuleLabel l r => Grammar.Language l r -> SSChLanguage l r
langToChLang lang = map (\(Grammar.Rule vars kids parent)
      -> SSChangeRule vars (map metaInject kids) (metaInject parent)) lang
--     -> hole "TODO: handle SortLabel, or keep generic?") lang
-- langToChLang lang = TotalMap.makeTotalMap case _ of
--     Hole -> Hole.hole "!TODO langToChLang rule for Hole"
--     HoleInterior -> Hole.hole "!TODO langToChLang rule for HoleInterior"
--     InjectRuleLabel r -> do
--         let Grammar.Rule vars kids parent = TotalMap.lookup r lang
--         Grammar.Rule vars (map metaInject kids) (metaInject parent)

-- TODO: everywhere that a boundary is introduced, I should have a function which does that, and only introduces it
-- if the change is not the identity.

-- wraps a boundary unless the change is the identity, in which case so is this function
wrapBoundary :: forall l r. Direction -> Grammar.SortChange l{-Expr.MetaChange l-} -> SSTerm l r -> SSTerm l r
wrapBoundary dir ch t = if isId ch then t else Expr.Expr (Boundary dir ch) [t]

-- Down rule that steps boundary through form - defined generically for every typing rule!
defaultDown :: forall l r. Ord r => Expr.IsExprLabel l => SSChLanguage l r -> StepRule l r
defaultDown lang (Expr.Expr (Boundary Down ch) [Expr.Expr (Inject (Grammar.DerivLabel ruleLabel sort)) kids]) =
 let (SSChangeRule metaVars crustyKidGSorts crustyParentGSort) = TotalMap.lookup ruleLabel lang in
 let freshener = genFreshener metaVars in
 let kidGSorts = map (freshen freshener) crustyKidGSorts in
 let parentGSort = freshen freshener crustyParentGSort in
 -- TODO: is this the right check?
 -- if not (injectMetaHoleyExpr (fst (endpoints ch)) == sort) then Bug.bug "assertion failed: ch boundary didn't match sort in defaultDown" else
 if not ((fst (endpoints ch)) == sort) then Bug.bug "assertion failed: ch boundary didn't match sort in defaultDown" else
 do
     sub /\ chBackUp <- doOperation ch parentGSort
     let kidGSorts' = map (Expr.subMetaExpr sub) kidGSorts
     let kidsWithBoundaries = Array.zipWith (\ch' kid -> wrapBoundary Down ch' kid) kidGSorts' kids
     let newSort = fst (endpoints chBackUp)
     -- pure $ wrapBoundary Up chBackUp $ Expr.Expr (Inject (Grammar.DerivLabel ruleLabel (injectMetaHoleyExpr newSort))) kidsWithBoundaries
     pure $ wrapBoundary Up chBackUp $ Expr.Expr (Inject (Grammar.DerivLabel ruleLabel newSort)) kidsWithBoundaries
defaultDown _ _ = Nothing

-- finds an element of a list satisfying a property, and splits the list into the pieces before and after it
getFirst :: forall t a. List t -> (t -> Maybe a) -> Maybe (List t /\ a /\ List t)
getFirst Nil _f = Nothing
getFirst (x : xs) f = case f x of
 Nothing ->
     do ts1 /\ a /\ ts2 <- getFirst xs f
        pure $ ((x : ts1) /\ a /\ ts2)
 Just a -> Just (Nil /\ a /\ xs)

defaultUp :: forall l r. Ord r => Expr.IsExprLabel l => SSChLanguage l r -> StepRule l r
defaultUp lang (Expr.Expr (Inject (Grammar.DerivLabel ruleLabel sort)) kids) =
 let (SSChangeRule metaVars crustyKidGSorts crustyParentGSort) = TotalMap.lookup ruleLabel lang in
 let freshener = genFreshener metaVars in
 let kidGSorts = map (freshen freshener) crustyKidGSorts in
 let parentGSort = freshen freshener crustyParentGSort in
 let findUpBoundary = case _ of
         Expr.Expr (Boundary Up ch) [kid] /\ sort1 -> Just (ch /\ kid /\ sort1)
         _ /\ _ -> Nothing
 in
 do
     (leftKidsAndSorts /\ (ch /\ kid /\ gSort) /\ rightKidsAndSorts)
         <- getFirst ((List.fromFoldable (Array.zip kids kidGSorts))) findUpBoundary
     sub /\ chBackDown <- doOperation ch gSort
     let wrapKid (kid1 /\ gSort1) = wrapBoundary Down (Expr.subMetaExpr sub gSort1) kid1
     let leftKids = map wrapKid leftKidsAndSorts
     let rightKids = map wrapKid rightKidsAndSorts
     let parentBoundary node = wrapBoundary Up (Expr.subMetaExpr sub parentGSort) node
     pure $ parentBoundary
         (Expr.Expr
             -- (Inject (Grammar.DerivLabel ruleLabel (injectMetaHoleyExpr (fst (endpoints chBackDown)))))
             (Inject (Grammar.DerivLabel ruleLabel (fst (endpoints chBackDown))))
             (Array.fromFoldable leftKids <> [wrapBoundary Down chBackDown kid] <> Array.fromFoldable rightKids))
defaultUp _ _ = Nothing

-------------- Other typechange related functions ---------------------

-- the input sort is the bottom sort
getPathChange :: forall l r. Ord r => Expr.IsExprLabel l => Grammar.IsRuleLabel l r => Grammar.LanguageChanges l r -> Grammar.DerivPath Dir.Up l r -> Grammar.Sort l -> Grammar.SortChange l
getPathChange _lang (Expr.Path Nil) bottomSort = inject bottomSort
---- getPathChange lang (Expr.Path ((Expr.Tooth (Grammar.DerivHole sort1) (ZipList.Path {left})) : path)) sort = unsafeCrashWith "Holes aren't paths"
----getPathChange lang (Expr.Path ((Expr.Tooth dlabel (ZipList.Path {left})) : path)) sort | isHoleDerivLabel dlabel = unsafeCrashWith "Holes aren't paths"
getPathChange lang (Expr.Path ((Expr.Tooth (Grammar.DerivString _) _) : _)) _ = unsafeCrashWith "Strings aren't paths"
getPathChange lang (Expr.Path ((Expr.Tooth (Grammar.DerivLabel r topSort) (ZipList.Path {left})) : path)) bottomSort =
    let Grammar.ChangeRule vars crustyKidChanges = TotalMap.lookup r lang in
    let crustyKidChange = fromJust' "Array.index crustyKidChanges (Rev.length left)" $ Array.index crustyKidChanges (Rev.length left) in
    let freshener = genFreshener vars in
    let kidChange = freshen freshener crustyKidChange in
    let leftType = snd $ endpoints kidChange in
    let (_ /\ sub) = fromJust' "unification shouldn't fail here: type error" $ unify leftType topSort in
    -- TODO: this should only substitute metavars in leftType, not in sort. I need to figure out how to codify that assumption in the code
    let kidChange' = subSomeMetaChange sub kidChange in
    let restOfPathChange = (getPathChange lang (Expr.Path path) (snd (endpoints kidChange'))) in
    trace ("in getPathChange, calling compose with " <> pretty kidChange' <> " and " <> pretty restOfPathChange) \_ ->
    trace ("to put it another way, calling with " <> show kidChange' <> " and " <> show restOfPathChange) \_ ->
    compose kidChange' restOfPathChange