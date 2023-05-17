module Language.Pantograph.Generic.ZipperMovement where

import Data.Either.Nested
import Data.Expr
import Data.Tuple.Nested
import Prelude
import Type.Direction
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Variant (case_, on)
import Hole as Hole

-- data MoveDir
--   = MoveUp | MoveDown | MoveLeft | MoveRight 
--   | MovePrev | MoveNext

moveZipper :: forall l. MoveDir -> Zipper l -> Maybe (Zipper l)
-- moveZipper MoveNext  z = Nothing
moveZipper = case_
  # on _up (\_ -> map snd <<< zipUp)
  # on _down (\_ -> map snd <<< Array.head <<< zipDowns)
  # on _left (\_ -> zipLeft)
  # on _right (\_ -> zipRight)
  # on _prev (\_ -> Hole.hole "moveZipper prev")
  # on _next (\_ -> Hole.hole "moveZipper next")

moveZipperp :: forall l. MoveDir -> Zipperp l -> Maybe (Zipper l \/ Zipperp l)
moveZipperp dir zipperp = do
  zipperp' <- moveZipperp' dir zipperp
  Just $ normalizeZipperp zipperp'

-- | Normalize a Zipperp by turning it into a Zipper if it has an empty
-- | selection.
normalizeZipperp :: forall l. Zipperp l -> Zipper l \/ Zipperp l
normalizeZipperp zipperp@(Zipperp path selection expr) = case selection of
  Left (Path Nil) -> Left (Zipper path expr)
  Right (Path Nil) -> Left (Zipper path expr)
  _ -> Right zipperp

moveZipperp' :: forall l. MoveDir -> Zipperp l -> Maybe (Zipperp l)
moveZipperp' = case_
  # on _up (\_ (Zipperp path selection expr) -> case selection of
      Left downPath -> do
        th /\ path' <- unstepPath path
        Just (Zipperp path' (Left (stepPath th downPath)) expr)
      Right upPath -> do
        th /\ upPath' <- unstepPath upPath
        Just (Zipperp path (Right upPath') (unTooth th expr))
    )
  # on _down (\_ (Zipperp path selection expr) -> 
      case selection of
        Left downPath -> do
          th /\ downPath' <- unstepPath downPath
          Just (Zipperp (stepPath th path) (Left downPath') expr)
        Right upPath -> do
          th /\ expr' <- tooth 0 expr
          Just (Zipperp path (Right (stepPath th upPath)) expr')
    )
  # on _left (\_ (Zipperp path selection expr) ->
      case selection of
        Right upPath -> do
          Tooth l kidsZip /\ upPath' <- unstepPath upPath
          expr' /\ kidsZip' <- ZipList.zipLeft (expr /\ kidsZip)
          Just (Zipperp path (Right (stepPath (Tooth l kidsZip') upPath')) expr')
        Left _ -> Nothing -- can't zip left/right when selecting up
    )
  # on _right (\_ (Zipperp path selection expr) ->
      case selection of
        Right upPath -> do
          Tooth l kidsZip /\ upPath' <- unstepPath upPath
          expr' /\ kidsZip' <- ZipList.zipRight (expr /\ kidsZip)
          Just (Zipperp path (Right (stepPath (Tooth l kidsZip') upPath')) expr')
        Left _ -> Nothing -- can't zip left/right when selecting up
    )
  # on _prev (\_ -> Hole.hole "moveZipperp' prev")
  # on _next (\_ -> Hole.hole "moveZipperp' next")
