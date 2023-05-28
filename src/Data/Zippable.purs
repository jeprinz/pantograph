module Data.Zippable where

import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))

class Zippable a where
  zipDowns :: a -> Array a
  zipUp' :: a -> Maybe (Int /\ a)
  -- !TODO isValidCursor :: a -> Boolean
  -- !TODO isValidSelect :: ? -> Boolean

zipUp :: forall a. Zippable a => a -> Maybe a
zipUp = map snd <<< zipUp'

zipDown :: forall a. Zippable a => Int -> a -> Maybe a
zipDown i a = zipDowns a Array.!! i

zipLeft :: forall a. Zippable a => a -> Maybe a
zipLeft a = do
  i /\ p <- zipUp' a
  zipDowns p Array.!! (i - 1)

zipRight :: forall a. Zippable a => a -> Maybe a
zipRight a = do
  i /\ p <- zipUp' a
  zipDowns p Array.!! (i + 1)

zipNext :: forall a. Zippable a => Int -> a -> Maybe a
zipNext i a = do
  let downs = zipDowns a
  case downs Array.!! i of
    Just a' -> Just a'
    Nothing -> case zipUp' a of
      Nothing -> Nothing
      Just (j /\ p) -> zipNext (j + 1) p

zipPrev :: forall a. Zippable a => a -> Maybe a
zipPrev a = do
  case zipUp' a of
    Nothing -> Nothing
    Just (j /\ p) -> do
      let downs = zipDowns p
      case downs Array.!! (j - 1) of
        Nothing -> Just p
        Just a' -> Just $ lastChild a'

lastChild :: forall a. Zippable a => a -> a
lastChild a = case Array.last (zipDowns a) of
  Nothing -> a
  Just a' -> lastChild a'
