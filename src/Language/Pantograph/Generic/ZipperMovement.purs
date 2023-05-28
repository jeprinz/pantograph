module Language.Pantograph.Generic.ZipperMovement where

import Data.Either.Nested
import Data.Expr
import Data.Tuple.Nested
import Prelude
import Type.Direction

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List.Rev as RevList
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Variant (case_, default, on)
import Data.Zippable as Zippable
import Debug (trace)
import Hole (hole)
import Hole as Hole
import Util (fromJust')

moveZipper :: forall l. MoveDir -> Zipper l -> Maybe (Zipper l)
moveZipper = case_
  # on _up (\_ -> Zippable.zipUp)
  # on _down (\_ -> Zippable.zipDown 0)
  # on _left (\_ -> Zippable.zipLeft)
  # on _right (\_ -> Zippable.zipRight)
  # on _prev (\_ -> Zippable.zipPrev)
  # on _next (\_ -> Zippable.zipNext 0)

moveZipperp :: forall l. MoveDir -> Zipperp l -> Maybe (Zipper l \/ Zipperp l)
moveZipperp dir = map normalizeZipperp <<< (case_ 
  # on _up (\_ -> Zippable.zipUp)
  # on _down (\_ -> Zippable.zipDown 0)
  # on _left (\_ -> Zippable.zipLeft)
  # on _right (\_ -> Zippable.zipRight)
  # on _prev (\_ -> Zippable.zipPrev)
  # on _next (\_ -> Zippable.zipNext 0)
  $ dir)

-- {-
-- I think this can be written in terms of ZipList.zipLeft and zipRight instead
-- -}
-- zipNext :: forall l. Int -> Zipper l -> Maybe (Zipper l)
-- zipNext kidSkip zip =
--     let children = zipDowns zip in
--     case Array.index children kidSkip of
--         Just (_ /\ child) -> Just child
--         Nothing -> case zipUp zip of
--             Just ((Tooth _ zipList) /\ parent) -> zipNext (ZipList.leftLength zipList + 1) parent
--             Nothing -> Nothing

-- zipPrev :: forall l. Zipper l -> Maybe (Zipper l)
-- zipPrev zip@(Zipper _ expr) =
--     case zipUp zip of
--         Nothing -> Nothing
--         Just (Tooth me zipList /\ parent) -> case ZipList.zipLeft (expr /\ zipList) of
--             Nothing -> Just parent
--             Just th ->
--                 let prevChild = snd $ fromJust' "zipPrev" $ Array.index (zipDowns parent) (ZipList.leftLength zipList - 1) in -- (Hole.hole "need to use th and parent to get a new position somehow") in
--                 Just $ lastChild prevChild

-- lastChild :: forall l. Zipper l -> Zipper l
-- lastChild zip =
--     let children = zipDowns zip in
--     case Array.index children (Array.length children - 1) of
--         Nothing -> zip
--         Just (_ /\ child) -> lastChild child


-- moveZipperp :: forall l. MoveDir -> Zipperp l -> Maybe (Zipper l \/ Zipperp l)
-- moveZipperp dir zipperp = do
--   zipperp' <- moveZipperp' dir zipperp
--   Just $ normalizeZipperp zipperp'

-- | Normalize a Zipperp by turning it into a Zipper if it has an empty
-- | selection.
normalizeZipperp :: forall l. Zipperp l -> Zipper l \/ Zipperp l
normalizeZipperp zipperp@(Zipperp path selection expr) = case selection of
  Left (Path Nil) -> Left (Zipper path expr)
  Right (Path Nil) -> Left (Zipper path expr)
  _ -> Right zipperp

-- moveZipperp' :: forall l. MoveDir -> Zipperp l -> Maybe (Zipperp l)
-- moveZipperp' = case_
--   # on _up (\_ (Zipperp path selection expr) -> case selection of
--       Left downPath -> do
--         th /\ path' <- unstepPath path
--         Just (Zipperp path' (Left (stepPath th downPath)) expr)
--       Right upPath -> do
--         th /\ upPath' <- unstepPath upPath
--         Just (Zipperp path (Right upPath') (unTooth th expr))
--     )
--   # on _down (\_ (Zipperp path selection expr) -> 
--       case selection of
--         Left downPath -> do
--           th /\ downPath' <- unstepPath downPath
--           Just (Zipperp (stepPath th path) (Left downPath') expr)
--         Right upPath -> do
--           th /\ expr' <- tooth 0 expr
--           Just (Zipperp path (Right (stepPath th upPath)) expr')
--     )
--   # on _left (\_ (Zipperp path selection expr) ->
--       case selection of
--         Right upPath -> do
--           Tooth l kidsZip /\ upPath' <- unstepPath upPath
--           expr' /\ kidsZip' <- ZipList.zipLeft (expr /\ kidsZip)
--           Just (Zipperp path (Right (stepPath (Tooth l kidsZip') upPath')) expr')
--         Left _ -> Nothing -- can't zip left/right when selecting up
--     )
--   # on _right (\_ (Zipperp path selection expr) ->
--       case selection of
--         Right upPath -> do
--           Tooth l kidsZip /\ upPath' <- unstepPath upPath
--           expr' /\ kidsZip' <- ZipList.zipRight (expr /\ kidsZip)
--           Just (Zipperp path (Right (stepPath (Tooth l kidsZip') upPath')) expr')
--         Left _ -> Nothing -- can't zip left/right when selecting up
--     )
--   # on _prev (\_ -> Hole.hole "moveZipperp' prev")
--   # on _next (\_ -> Hole.hole "moveZipperp' next")
