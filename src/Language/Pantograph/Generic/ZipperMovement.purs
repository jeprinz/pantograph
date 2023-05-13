module Language.Pantograph.Generic.ZipperMovement where

import Data.Either.Nested
import Data.Expr
import Data.Tuple.Nested
import Prelude
import Type.Direction

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
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

moveZipperP :: forall l. MoveDir -> ZipperP l -> Maybe (Zipper l \/ ZipperP l)
moveZipperP dir zipperp = do
  zipperp' <- moveZipperP' dir zipperp
  Just $ normalizeZipperP zipperp'

-- | Normalize a ZipperP by turning it into a Zipper if it has an empty
-- | selection.
normalizeZipperP :: forall l. ZipperP l -> Zipper l \/ ZipperP l
normalizeZipperP zipperp@(ZipperP zp) = case zp.selection of
  Left (Path Nil) -> Left (Zipper {path: zp.path, expr: zp.expr})
  Right (Path Nil) -> Left (Zipper {path: zp.path, expr: zp.expr})
  _ -> Right zipperp

moveZipperP' :: forall l. MoveDir -> ZipperP l -> Maybe (ZipperP l)
moveZipperP' = case_
  # on _up (\_ (ZipperP zp) -> case zp.selection of
      Left downPath -> do
        th /\ path' <- unstepPath zp.path
        Just (ZipperP zp {path = path', selection = Left (stepPath th downPath)})
      Right upPath -> do
        th /\ upPath' <- unstepPath upPath
        Just (ZipperP zp {selection = Right upPath', expr = unTooth th zp.expr})
    )
  # on _down (\_ (ZipperP zp) -> 
      case zp.selection of
        Left downPath -> do
          th /\ downPath' <- unstepPath downPath
          Just (ZipperP zp {path = stepPath th zp.path, selection = Left downPath'})
        Right upPath -> do
          -- th /\ upPath' <- 
          -- Just (Right (ZipperP zp {selection = Right upPath', expr = unTooth th zp.expr}))
          -- {head: th /\ zipper} <- Array.uncons (zipDowns (Zipper {path: upPath, expr: zp.expr}))
          -- Expr.too
          th /\ expr <- tooth 0 zp.expr
          Just (ZipperP zp {selection = Right (stepPath th upPath), expr = expr})
    )
  # on _left (\_ -> unsafeCrashWith "!TODO moveZipperP' left")
  # on _right (\_ -> unsafeCrashWith "!TODO moveZipperP' right")
  # on _prev (\_ -> unsafeCrashWith "!TODO moveZipperP' prev")
  # on _next (\_ -> unsafeCrashWith "!TODO moveZipperP' next")
