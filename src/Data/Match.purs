module Data.Match where

import Data.Either.Nested
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Control.Monad.Writer (WriterT, execWriterT, mapWriterT, runWriterT, tell)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tree (Tree(..))
import Data.Tuple (uncurry)
import Data.Variant (Variant)
import Text.Pretty (class Pretty, parens)

data Pattern ctr
  = Constr ctr (Array (Pattern ctr))
  | Var
  | Wild

infix 7 Constr as %

derive instance Generic (Pattern ctr) _

-- | The `Show` instance for `Pattern` uses the infixed version of `Constr`.
instance Show ctr => Show (Pattern ctr) where
  show = case _ of
    Constr ctr kids -> parens $ show ctr <> " % " <> show kids
    Var -> "var"
    Wild -> "wild"

instance Show ctr => Pretty (Pattern ctr) where
  pretty = show

-- | The type of matching functions such that "`a` is matched by `Pattern`s with
-- | contructor `ctr`, with resulting matches of type `m`".
type Match a ctr m = Pattern ctr -> a -> MatchM m
type MatchM m = WriterT (Array m) Maybe Unit

matches :: forall a ctr m b. Match a ctr m -> Array (Pattern ctr /\ (Array m -> Maybe b)) -> a -> b
matches match cases a = case Array.uncons cases of
  Nothing -> bug $ "matches: no patterns matched"
  Just {head: pat /\ k, tail: cases'} -> case execWriterT $ match pat a of
    Nothing -> matches match cases' a
    Just m -> case k m of
      Nothing -> matches match cases' a
      Just b -> b

mapMatches :: forall m m'. (m -> m') -> MatchM m -> MatchM m'
mapMatches f = mapWriterT (map (map (map f)))

matchTree :: forall a b mNode. Match a b mNode -> Match (Tree a) (Pattern b) (mNode \/ Tree a)
matchTree matchNode (a % kids) (Tree a' kids') = do
  mapMatches Left $ matchNode a a'
  uncurry (matchTree matchNode) `traverse_` Array.zip kids kids'
matchTree _ Var t = mapMatches Right $ tell [t]
matchTree _ Wild _ = pure unit

-- instance Matchable (Tree a) a (Array (String /\ Tree a)) where
--   match (a % kids) (Tree a' kids') = ?a
--   match (Var x) tree = x ≔ tree
--   match Wild tree = pure unit

-- instance Pretty Pattern where
--   pretty = case _ of
--     Constr {name, args} -> "(" <> name <> " " <> Array.intercalate " " (pretty <$> args) <> ")"
--     Var x -> "$" <> x
--     Wild -> "_"

-- type ParseM = ExceptT String (State String)

-- parsePattern :: String -> Pattern
-- parsePattern str0 = case runState (runExceptT parse) str0 of
--   Left err /\ str1 -> bug $ "parsePattern: " <> err <> "\n" <> "at: ... " <> show str1
--   Right t /\ _str1 -> t
--   where

--   -- parse word and erase trailing whitespace
--   word :: ParseM (Maybe String)
--   word = do
--     word <- gets $ String.takeWhile Char.isAlphaNum
--     if String.length word == 0 then
--       pure Nothing
--     else do
--       modify_ $ String.dropWhile Char.isSpace <<< String.drop (String.length word)
--       pure (Just word)

--   -- parse exact string
--   string :: String -> ParseM (Maybe String)
--   string s = do
--     str <- get
--     case String.stripPrefix (Pattern s) str of
--       Nothing -> pure Nothing -- bug $ "parsePattern: expected string " <> show s <> " at " <> show str
--       Just str' -> do
--         put $ String.dropWhile Char.isSpace str'
--         pure (Just s)

--   var :: ParseM (Maybe Pattern)
--   var = 
--     string "$" >>= case _ of
--       Nothing -> pure Nothing
--       Just _ -> 
--         word >>= case _ of
--           Nothing -> throwError $ "expected var name"
--           Just w -> pure $ Just $ Var w

--   wild :: ParseM (Maybe Pattern)
--   wild =
--     string "_" >>= case _ of
--       Nothing -> pure Nothing
--       Just _ -> pure $ Just Wild

--   matchString :: String -> ParseM (Maybe String)
--   matchString s = do
--     str <- get
--     case String.stripPrefix (Pattern s) str of
--       Nothing -> pure Nothing
--       Just _ -> pure (Just s)

--   trees :: ParseM (Array Pattern)
--   trees = matchString ")" >>= case _ of
--     Nothing -> pure mempty
--     Just _ -> tree >>= case _ of
--       Nothing -> throwError $ "expected argument"
--       Just t -> Array.cons t <$> trees

--   tree :: ParseM (Maybe Pattern)
--   tree =
--     string "(" >>= case _ of
--       Nothing -> pure Nothing
--       Just _ -> word >>= case _ of
--         Nothing -> throwError $ "expected constructor name"
--         Just name -> do
--           args <- trees
--           string "(" >>= case _ of
--             Nothing -> throwError $ "expected closing parenthesis"
--             Just _ -> pure $ Just $ Constr {name, args}

--   or p p' = p >>= case _ of 
--     Just x -> pure x
--     Nothing -> p'

--   parse :: ParseM Pattern
--   parse =
--     or var $
--     or wild $
--     or tree $
--     throwError "expected tree"

-- -- | Try to match an `a` to a `String`, storing results in `matches`.
-- match :: forall a matches.
--   {matchConstr :: a -> Pattern -> StateT matches Maybe Unit, emptyMatches :: matches} ->
--   String -> a -> Maybe matches
-- match {matchConstr, emptyMatches} str a = execStateT (matchConstr a (parsePattern str)) emptyMatches

