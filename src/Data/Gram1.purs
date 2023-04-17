module Data.Gram1 where

import Data.Tuple.Nested
import Prelude

import Control.Monad.Reader (Reader, asks, local, runReader)
import Control.Plus (empty)
import Data.Array as Array
import Data.Const (Const)
import Data.Functor.Compose (Compose(..))
import Data.Generic.Rep (class Generic)
import Data.Isomorphism (class Isomorphism, from, to)
import Data.List (List, (:))
import Data.List as List
import Data.List.Drv (DrvList, singletonLeft, singletonRight, underiveAround)
import Data.List.Rev (RevList, reverse, unreverse, (:*))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', maybe')
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable, sequence, traverse)
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, indent, pretty, (<+>))

-- | Gram
newtype Gram val joint = Gram (Gram' val joint (Gram val joint))
type Gram' val joint gram = {val :: val, joint :: joint gram}

-- mapGramJoint f (Gram g') = Gram g' {joint = f <$> g'.joint}
-- mapGram'Joint f g' = g' {joint = f <$> g'.joint}

derive instance Generic (Gram val joint) _
instance (Show val, Functor joint, Show (joint String)) => Show (Gram val joint) where show (Gram g') = "{val: " <> show g'.val <> ", joint: " <> show (show <$> g'.joint) <> "}"

traverseGram :: forall val val' joint m.
  Functor joint => Monad m =>
  (Gram' val joint (m (Gram val' joint)) -> m (Gram' val' joint (Gram val' joint))) ->
  Gram val joint -> m (Gram val' joint)
traverseGram f (Gram {val, joint}) = Gram <$> f {val, joint: traverseGram f <$> joint}

renameGram :: forall proto x y joint.
  Ord x => Functor joint =>
  (Gram' (proto x) joint (Reader (Map.Map x y) (Gram (proto y) joint)) -> Reader (Map.Map x y) (Gram' (proto y) joint (Gram (proto y) joint))) ->
  Gram (proto x) joint -> (Gram (proto y) joint)
renameGram rho = flip runReader Map.empty <<< traverseGram rho

-- | Expr
type Expr val link = Gram val (Compose Array link)

prettyExpr :: forall val link. Functor link => Pretty val => Pretty (link String) => Expr val link -> String
prettyExpr (Gram {val, joint: Compose kids}) = "(" <> pretty val <> " " <> Array.intercalate " " (pretty <<< map prettyExpr <$> kids) <> ")"

-- | Path
type Path val link = Maybe (Gram (val /\ DrvList (Expr val link)) (Compose link Maybe))

appendPath :: forall val link. Functor link => Path val link -> Path val link -> Path val link
appendPath p1 Nothing = p1
appendPath p1 (Just (Gram {val, joint: Compose link_path})) = Just (Gram {val, joint: Compose (map (appendPath p1) link_path)})

reversePath :: forall val link. Functor link => Isomorphism link => Path val link -> Path val link
reversePath = go Nothing
  where 
  go :: Path val link -> Path val link -> Path val link
  go p1 Nothing = p1
  go p1 (Just (Gram {val: val /\ kids, joint: Compose link_p2})) =
    go (Just (Gram {val: val /\ kids, joint: Compose (map (const p1) link_p2)})) (from link_p2)

-- prettyUpPath :: forall val link. Pointed link => Functor link => Pretty val => Pretty (link String) => Path val link -> String
-- prettyUpPath = go "⌶" <<< to
--   where
--   go :: String -> link (Path val link) -> String
--   go down = pretty <<< map case _ of 
--     Nothing -> down 
--     Just (Gram {val: val /\ kids, joint: Compose link_path}) -> go ("(" <> pretty val <> " " <> List.intercalate " " (underiveAround (prettyExpr <$> kids) (pure down)) <> ")") link_path

prettyUpPath :: forall val link. Isomorphism link => Functor link => Pretty val => Pretty (link String) => Path val link -> String
prettyUpPath = prettyDownPath <<< reversePath

prettyDownPath :: forall val link. Isomorphism link => Functor link => Pretty val => Pretty (link String) => Path val link -> String
prettyDownPath = go <<< to
  where 
  go :: link (Path val link) -> String
  go = pretty <<< map case _ of 
    Nothing -> "⌶"
    Just (Gram {val: val /\ kids, joint: Compose link_path}) -> "(" <> pretty val <> " " <> List.intercalate " " (underiveAround (prettyExpr <$> kids) (pure (go link_path))) <> ")"

-- | Change
-- type Change val = Gram val (ChangeJoint val)
-- data ChangeJoint val gram
--   = Plus {}

--
-- test
--

data Proto var = Ref var | Lam var | App | Let var
derive instance Generic (Proto var) _
instance Show var => Show (Proto var) where show x = genericShow x
derive instance Functor Proto
derive instance Foldable Proto
derive instance Traversable Proto

instance Pretty var => Pretty (Proto var) where
  pretty (Ref v) = pretty v
  pretty (Lam v) = "λ" <+> pretty v
  pretty App = ""
  pretty (Let v) = "let" <+> pretty v

data Link a = IndentedLink (Link a) | PureLink a
derive instance Generic (Link a) _
instance Show a => Show (Link a) where show x = genericShow x
derive instance Functor Link
derive instance Foldable Link
derive instance Traversable Link
instance Isomorphism Link where 
  to = PureLink
  from (IndentedLink link) = from link
  from (PureLink a) = a

instance Pretty a => Pretty (Link a) where 
  pretty (IndentedLink a) = indent ("\n" <> pretty a)
  pretty (PureLink a) = pretty a

data Var = Var String
derive instance Generic Var _
derive instance Eq Var
derive instance Ord Var
instance Show Var where show x = genericShow x
instance Pretty Var where
  pretty (Var str) = str 

-- constructors
var v = Gram {val: Ref v, joint: wrap mempty} :: Expr (Proto Var) Link
lam v e = Gram {val: Lam v, joint: wrap (to <$> [e])} :: Expr (Proto Var) Link
app e1 e2 = Gram {val: App, joint: wrap (to <$> [e1, e2])} :: Expr (Proto Var) Link
let_ v e1 e2 = Gram {val: Let v, joint: wrap (to <$> [e1, e2])} :: Expr (Proto Var) Link

lam_bod v bod = Just (Gram {val: Lam v /\ mempty, joint: Compose (to bod)}) :: Path (Proto Var) Link
app_arg arg bod = Just (Gram {val: App /\ singletonRight bod, joint: Compose (to arg)}) :: Path (Proto Var) Link
app_bod arg bod = Just (Gram {val: App /\ singletonLeft arg, joint: Compose (to bod)}) :: Path (Proto Var) Link
let_imp v imp bod = Just (Gram {val: Let v /\ singletonRight bod, joint: Compose (to imp)}) :: Path (Proto Var) Link
let_bod v imp bod = Just (Gram {val: Let v /\ singletonLeft imp, joint: Compose (to bod)}) :: Path (Proto Var) Link

-- examples
ex_path1 = lam_bod (Var "x") $ lam_bod (Var "y") $ Nothing
ex_path2 = app_arg (ex_path1 `appendPath` app_bod (var (Var "w")) ex_path1) (var (Var "z"))

-- -- renaming
-- rename :: Expr (Proto Var) Link -> Expr (Proto Var) Link
-- rename = renameGram \{val, joint} -> case val /\ unwrap joint of
--   Ref v /\ [] -> {val: _, joint: empty} <$> asks (Ref <<< fromMaybe' (\_ -> unsafeCrashWith $ "unknown variable: " <> show v) <<< Map.lookup v)
--   Lam v /\ [e1] -> local (Map.insert v v) do {val, joint: _} <$> map ?a (sequence [from e1])
--   App /\ [e1, e2] -> {val, joint: _} <$> ?a
--   Let v /\ [e1, e2] -> ?a
  
--   -- local (Map.insert v v) do {val, joint: _} <<< map ?a <$> sequence joint
--   _ -> unsafeCrashWith "invalid"
