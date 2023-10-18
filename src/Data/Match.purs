module Data.Match where

import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Control.Monad.Except (ExceptT, runExcept, runExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.State (State, StateT, evalState, evalStateT, execState, execStateT, get, gets, modify, modify_, put, runState, runStateT)
import Data.Array as Array
import Data.CodePoint.Unicode as Char
import Data.Either (Either(..))
import Data.Foldable (and)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Hole (hole)
import Text.Pretty (class Pretty, pretty)

data MatchTree = Construction {constr :: String, args :: Array MatchTree} | Var String

derive instance Generic MatchTree _
instance Show MatchTree where show x = genericShow x

instance Pretty MatchTree where
  pretty = case _ of
    Construction {constr, args} -> "(" <> constr <> " " <> Array.intercalate " " (pretty <$> args) <> ")"
    Var x -> "$" <> x

type ParseM = ExceptT String (State String)

parseMatchTree :: String -> MatchTree
parseMatchTree str0 = case runState (runExceptT parse) str0 of
  Left err /\ str1 -> bug $ "parseMatchTree: " <> err <> "\n" <> "at: ... " <> show str1
  Right t /\ _str1 -> t
  where

  -- parse word and erase trailing whitespace
  word :: ParseM (Maybe String)
  word = do
    word <- gets $ String.takeWhile Char.isAlphaNum
    if String.length word == 0 then
      pure Nothing
    else do
      modify_ $ String.dropWhile Char.isSpace <<< String.drop (String.length word)
      pure (Just word)

  -- parse exact string
  string :: String -> ParseM (Maybe String)
  string s = do
    str <- get
    case String.stripPrefix (Pattern s) str of
      Nothing -> pure Nothing -- bug $ "parseMatchTree: expected string " <> show s <> " at " <> show str
      Just str' -> do
        put $ String.dropWhile Char.isSpace str'
        pure (Just s)

  var :: ParseM (Maybe MatchTree)
  var = 
    string "$" >>= case _ of
      Nothing -> pure Nothing
      Just _ -> 
        word >>= case _ of
          Nothing -> throwError $ "expected var name"
          Just w -> pure $ Just $ Var w

  matchString :: String -> ParseM (Maybe String)
  matchString s = do
    str <- get
    case String.stripPrefix (Pattern s) str of
      Nothing -> pure Nothing
      Just _ -> pure (Just s)

  trees :: ParseM (Array MatchTree)
  trees = matchString ")" >>= case _ of
    Nothing -> pure mempty
    Just _ -> tree >>= case _ of
      Nothing -> throwError $ "expected argument"
      Just t -> Array.cons t <$> trees

  tree :: ParseM (Maybe MatchTree)
  tree =
    string "(" >>= case _ of
      Nothing -> pure Nothing
      Just _ -> word >>= case _ of
        Nothing -> throwError $ "expected constructor name"
        Just constr -> do
          args <- trees
          string "(" >>= case _ of
            Nothing -> throwError $ "expected closing parenthesis"
            Just _ -> pure $ Just $ Construction {constr, args}

  parse :: ParseM MatchTree
  parse = 
    var >>= case _ of
      Just x -> pure x
      Nothing -> tree >>= case _ of
        Just t -> pure t
        Nothing -> throwError $ "expected tree (var or construction)"

-- | Try to match an `a` to a `String`, storing results in `matches`.
match :: forall a matches.
  {matchConstr :: a -> MatchTree -> StateT matches Maybe Unit, emptyMatches :: matches} ->
  String -> a -> Maybe matches
match {matchConstr, emptyMatches} str a = execStateT (matchConstr a (parseMatchTree str)) emptyMatches
