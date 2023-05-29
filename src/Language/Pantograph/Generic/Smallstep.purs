module Language.Pantograph.Generic.Smallstep where

import Data.Foldable
import Language.Pantograph.Generic.ChangeAlgebra
import Language.Pantograph.Generic.Unification
import Prelude hiding (compose)

import Bug as Bug
import Bug.Assertion (Assertion(..), assert, makeAssertionBoolean)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Expr ((%), (%<))
import Data.Expr as Expr
import Data.List (List(..), (:))
import Data.List as List
import Data.Set (Set)
import Data.List.Rev as Rev
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.TotalMap as TotalMap
import Data.Tuple (snd, fst)
import Data.Tuple.Nested (type (/\), (/\))
import Hole (hole)
import Hole as Hole
import Language.Pantograph.Generic.ChangeAlgebra (endpoints)
import Language.Pantograph.Generic.Grammar (class IsRuleLabel, isHoleDerivLabel, isHoleDerivTerm)
import Language.Pantograph.Generic.Grammar as Grammar
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (pretty)
import Type.Direction as Dir
import Util (lookup', fromJust')
import Utility ((<$$>))
import Data.TotalMap (TotalMap)
import Data.TotalMap as TotalMap

data Direction = Up | Down -- TODO:

data StepExprLabel l r = Inject (Grammar.DerivLabel l r) | Cursor | Boundary Direction (Grammar.SortChange l) -- (Expr.MetaChange l)
type SSTerm l r = Expr.Expr (StepExprLabel l r)

type StepRule l r = SSTerm l r -> Maybe (SSTerm l r)

-- ---------- Code for converting zippers to terms and back ------------------------------------------

addToothToTerm :: forall l r. Expr.Tooth (Grammar.DerivLabel l r) -> SSTerm l r -> SSTerm l r
addToothToTerm (Expr.Tooth l (ZipList.Path {left, right})) t =
 Expr.Expr (Inject l) $
     Array.fromFoldable (map (map Inject) (Rev.unreverse left)) <>
     [t] <>
     Array.fromFoldable (map (map Inject) right)

zipperToTerm :: forall l r. Expr.Path Dir.Up (Grammar.DerivLabel l r) -> Grammar.DerivTerm l r -> SSTerm l r
zipperToTerm (Expr.Path Nil) exp = Expr.Expr Cursor [map Inject exp]
zipperToTerm (Expr.Path (th : path)) exp = addToothToTerm th (zipperToTerm (Expr.Path path) exp)

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

termToZipper :: forall l r. SSTerm l r -> (Expr.Path Dir.Up (Grammar.DerivLabel l r) /\ Grammar.DerivTerm l r)
termToZipper (Expr.Expr Cursor [kid]) =
 Expr.Path Nil /\ (assertJustExpr kid)
termToZipper (Expr.Expr (Inject l) kids) =
 let kids' = List.fromFoldable $ map termToZipper kids in
 let isPath (p /\ e) = case p of
         (Expr.Path Nil) -> Right e
         _ -> Left (p /\ e) in
 let pathOrNot = oneOrNone kids' isPath in
 case pathOrNot of
     -- child didn't have cursor
     Left kids'' -> Expr.Path Nil /\ Expr.Expr l (Array.fromFoldable (kids''))
     -- child has exactly one cursor
     Right (leftKids /\ (Expr.Path p /\ e) /\ rightKids) ->
         let newTooth = l %< ZipList.Path {left: Rev.reverse leftKids, right: rightKids} in
         Expr.Path (newTooth : p) /\ e
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

step :: forall l r. SSTerm l r -> List (StepRule l r) -> Maybe (SSTerm l r)
step t@(Expr.Expr l kids) rules =
 case doAnyApply t rules of
     Nothing -> do
         kids' <- stepSomebody (List.fromFoldable kids) rules
         pure $ Expr.Expr l (Array.fromFoldable kids')
     Just t' -> Just t'


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

type ChangeSort l = Expr.Expr (Expr.Meta (Expr.ChangeLabel (Expr.Meta (Grammar.SortLabel l))))
data ChangeRule l = ChangeRule (Set Expr.MetaVar) (Array (ChangeSort l)) (ChangeSort l)

-- The sorts in these are Expr (Meta (ChangeLabel (Meta l)).
-- the out Meta is the one that comes from the normal rules. The (ChangeLabel (Meta l))
-- corresponds with the changes in the boundaries, which are Expr (ChangeLabel (Meta l))
--type ChLanguage l r = Grammar.Language (Expr.ChangeLabel (Expr.Meta l)) r
type ChLanguage l r = TotalMap r (ChangeRule l)

metaInject :: forall l. Expr.MetaExpr l -> Expr.MetaExpr (Expr.ChangeLabel (Expr.Meta l))
metaInject e = map (map (Expr.Inject <<< Expr.Meta <<< Right)) e

langToChLang :: forall l r. IsRuleLabel l r => Grammar.Language l r -> ChLanguage l r
langToChLang lang = map (\(Grammar.Rule vars kids parent)
      -> ChangeRule vars (map metaInject kids) (metaInject parent)) lang
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
defaultDown :: forall l r. Ord r => Expr.IsExprLabel l => ChLanguage l r -> StepRule l r
defaultDown lang (Expr.Expr (Boundary Down ch) [Expr.Expr (Inject (Grammar.DerivLabel ruleLabel sort)) kids]) =
 let (ChangeRule metaVars crustyKidGSorts crustyParentGSort) = TotalMap.lookup ruleLabel lang in
 let freshener = genFreshener metaVars in
 let kidGSorts = map (freshen freshener) crustyKidGSorts in
 let parentGSort = freshen freshener crustyParentGSort in
 -- TODO: is this the right check?
 -- if not (injectMetaHoleyExpr (fst (endpoints ch)) == sort) then Bug.bug "assertion failed: ch boundary didn't match sort in defaultDown" else
 if not ((fst (endpoints ch)) == sort) then Bug.bug "assertion failed: ch boundary didn't match sort in defaultDown" else
 do
     sub /\ chBackUp <- doOperation ch parentGSort
     let kidGSorts' = map (Expr.subMetaExpr sub) kidGSorts
     let kidsWithBoundaries = (\ch' kid -> wrapBoundary Down ch' kid) <$> kidGSorts' <*> kids
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

defaultUp :: forall l r. Ord r => Expr.IsExprLabel l => ChLanguage l r -> StepRule l r
defaultUp lang (Expr.Expr (Inject (Grammar.DerivLabel ruleLabel sort)) kids) =
 let (ChangeRule metaVars crustyKidGSorts crustyParentGSort) = TotalMap.lookup ruleLabel lang in
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

getPathChange :: forall l r. Ord r => Expr.IsExprLabel l => Grammar.IsRuleLabel l r => Grammar.LanguageChanges l r -> Grammar.DerivPath Dir.Up l r -> Grammar.Sort l -> Grammar.SortChange l
getPathChange _lang (Expr.Path Nil) sort = inject sort
---- getPathChange lang (Expr.Path ((Expr.Tooth (Grammar.DerivHole sort1) (ZipList.Path {left})) : path)) sort = unsafeCrashWith "Holes aren't paths"
----getPathChange lang (Expr.Path ((Expr.Tooth dlabel (ZipList.Path {left})) : path)) sort | isHoleDerivLabel dlabel = unsafeCrashWith "Holes aren't paths"
getPathChange lang (Expr.Path ((Expr.Tooth (Grammar.DerivString _) _) : _)) _ = unsafeCrashWith "Strings aren't paths"
getPathChange lang (Expr.Path ((Expr.Tooth (Grammar.DerivLabel r sort1) (ZipList.Path {left})) : path)) sort =
  assert  (makeAssertionBoolean { condition: sort1 == sort -- injectMetaHoleyExpr sort
                                 , source: "getPathChange"
                                 , name: "matchingSorts"
                                 , message: "The sort of the current tooth's derivation label (sort) should match the input sort (sort1).\n  - sort = " <> pretty sort <> "\n  - sort1 = " <> pretty sort1}) \_ ->
    let Grammar.ChangeRule vars crustyKidChanges = TotalMap.lookup r lang in
    let crustyKidChange = fromJust' "Array.index crustyKidChanges (Rev.length left)" $ Array.index crustyKidChanges (Rev.length left) in
    let freshener = genFreshener vars in
    let kidChange = freshen freshener crustyKidChange in
    let leftType = snd $ endpoints kidChange in
    let (_ /\ sub) = fromJust' "unification shouldn't fail here: type error" $ unify leftType sort in
    -- TODO: this should only substitute metavars in leftType, not in sort. I need to figure out how to codify that assumption in the code
    let kidChange' = subSomeMetaChange sub kidChange in
    compose kidChange' (getPathChange lang (Expr.Path path) (snd (endpoints kidChange')))