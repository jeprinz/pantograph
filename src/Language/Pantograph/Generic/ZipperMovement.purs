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
import Partial.Unsafe (unsafeCrashWith)

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
  # on _prev (\_ -> unsafeCrashWith "!TODO moveZipper prev")
  # on _next (\_ -> unsafeCrashWith "!TODO moveZipper next")

moveZipperp :: forall l. MoveDir -> Zipperp l -> Maybe (Zipper l \/ Zipperp l)
moveZipperp dir zipperp = do
  zipperp' <- moveZipperp' dir zipperp
  Just $ normalizeZipperp zipperp'

-- | Normalize a Zipperp by turning it into a Zipper if it has an empty
-- | selection.
normalizeZipperp :: forall l. Zipperp l -> Zipper l \/ Zipperp l
normalizeZipperp zipperp@(Zipperp zp) = case zp.selection of
  Left (Path Nil) -> Left (Zipper {path: zp.path, expr: zp.expr})
  Right (Path Nil) -> Left (Zipper {path: zp.path, expr: zp.expr})
  _ -> Right zipperp

moveZipperp' :: forall l. MoveDir -> Zipperp l -> Maybe (Zipperp l)
moveZipperp' = case_
  # on _up (\_ (Zipperp zp) -> case zp.selection of
      Left downPath -> do
        th /\ path' <- unstepPath zp.path
        Just (Zipperp zp {path = path', selection = Left (stepPath th downPath)})
      Right upPath -> do
        th /\ upPath' <- unstepPath upPath
        Just (Zipperp zp {selection = Right upPath', expr = unTooth th zp.expr})
    )
  # on _down (\_ (Zipperp zp) -> 
      case zp.selection of
        Left downPath -> do
          th /\ downPath' <- unstepPath downPath
          Just (Zipperp zp {path = stepPath th zp.path, selection = Left downPath'})
        Right upPath -> do
          -- th /\ upPath' <- 
          -- Just (Right (Zipperp zp {selection = Right upPath', expr = unTooth th zp.expr}))
          -- {head: th /\ zipper} <- Array.uncons (zipDowns (Zipper {path: upPath, expr: zp.expr}))
          -- Expr.too
          th /\ expr <- tooth 0 zp.expr
          Just (Zipperp zp {selection = Right (stepPath th upPath), expr = expr})
    )
  # on _left (\_ (Zipperp zp) ->
      case zp.selection of
        Right upPath -> do
          Tooth l kidsZip /\ upPath' <- unstepPath upPath
          expr' /\ kidsZip' <- ZipList.zipLeft (zp.expr /\ kidsZip)
          Just (Zipperp zp {selection = Right (stepPath (Tooth l kidsZip') upPath'), expr = expr'})
        Left _ -> Nothing -- can't zip left/right when selecting up
    )
  # on _right (\_ (Zipperp zp) ->
      case zp.selection of
        Right upPath -> do
          Tooth l kidsZip /\ upPath' <- unstepPath upPath
          expr' /\ kidsZip' <- ZipList.zipRight (zp.expr /\ kidsZip)
          Just (Zipperp zp {selection = Right (stepPath (Tooth l kidsZip') upPath'), expr = expr'})
        Left _ -> Nothing -- can't zip left/right when selecting up
    )
  # on _prev (\_ -> unsafeCrashWith "!TODO moveZipperp' prev")
  # on _next (\_ -> unsafeCrashWith "!TODO moveZipperp' next")
