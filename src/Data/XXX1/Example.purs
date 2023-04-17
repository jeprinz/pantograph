module Data.XXX1.Example where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.List.Reversed (ReversedList, reverse)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (class Traversable)
import Data.XXX1 as Gram
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, pretty, (<+>))
import Type.Proxy (Proxy(..))

--
-- Proto
--

data Proto var 
  = Ref var
  | Lam var
  | App

derive instance Generic (Proto var) _
instance Show var => Show (Proto var) where show x = genericShow x
derive instance Functor Proto
derive instance Foldable Proto
derive instance Traversable Proto

instance Pretty (Proto String) where 
  pretty (Ref var) = var
  pretty (Lam var) = "λ" <+> var
  pretty App = ""

--
-- Expr
--

type Expr = Gram.Expr Proto Link Var
type ExprFix rec = Gram.ExprFix Proto Link Var rec

refExpr :: Var -> Expr
refExpr var = Gram.Gram {proto: Ref var, joint: Gram.ExprJoint []}

lamExpr :: Var -> Expr -> Expr
lamExpr var bod = Gram.Gram {proto: Lam var, joint: Gram.ExprJoint $ IdLink <$> [bod]}

appExpr :: Expr -> Expr -> Expr
appExpr apl arg = Gram.Gram {proto: App, joint: Gram.ExprJoint $ IdLink <$> [apl, arg]}

matchExpr :: forall a rec.
  { ref :: {var :: Var} -> a
  , lam :: {var :: Var, bod :: Link rec} -> a
  , app :: {apl :: Link rec, arg :: Link rec} -> a
  } ->
  ExprFix rec -> a
matchExpr f {proto: Ref var, joint: Gram.ExprJoint []} = f.ref {var}
matchExpr f {proto: Lam var, joint: Gram.ExprJoint [bod]} = f.lam {var, bod}
matchExpr f {proto: App, joint: Gram.ExprJoint [apl, arg]} = f.app {apl, arg}
matchExpr _ _ = unsafeCrashWith "matchExpr: invalid Expr"

-- instance PrettyContainer ExprFix where

ex1 :: Expr 
ex1 = lamExpr x (refExpr x)
  where x = Var "x"


--
-- Path
--

type Path dir = Gram.Path dir Proto Link Var
type Tooth = Gram.Tooth Proto Link Var

lam_bod :: forall link var proto link var. link (Gram.Gram Proto (Gram.PathJoint "up" proto link var) link var) -> var -> Gram.Gram Proto (Gram.PathJoint "up" proto link var) link var
lam_bod up x = Gram.Gram {proto: Lam x, joint: Gram.ConsPathJoint Proxy {lefts: mempty, rights: mempty} up}

app_arg :: forall link var proto link var. link (Gram.Gram Proto (Gram.PathJoint "up"proto link var) link var) -> Gram.Gram proto Gram.ExprJoint link var -> Gram.Gram Proto (Gram.PathJoint "up" proto link var) link var
app_arg up apl = Gram.Gram {proto: App, joint: Gram.ConsPathJoint Proxy {lefts: reverse (List.singleton apl), rights: mempty} up}

app_apl ∷ forall link var proto link var. link (Gram.Gram Proto (Gram.PathJoint "up" proto link var) link var) → Gram.Gram proto Gram.ExprJoint link var → Gram.Gram Proto (Gram.PathJoint "up" proto link var) link var
app_apl up arg = Gram.Gram {proto: App, joint: Gram.ConsPathJoint Proxy {lefts: mempty, rights: List.singleton arg} up}

pa1 :: Path "up"
pa1 = ?A `lam_bod` Var "x"


--
-- Change
--

type Change = Gram.Change Proto Link Var Metavar

--
-- Var
--

data Var = Var String

derive instance Generic Var _
instance Show Var where show x = genericShow x

instance Pretty Var where
  pretty (Var str) = str

--
-- Metavar
--

data Metavar = Metavar String

--
-- Link
--

data Link a 
  = IndentLink (Link a)
  | NewlineLink (Link a)
  | CommentLink String (Link a)
  | IdLink a

derive instance Generic (Link a) _
instance Show a => Show (Link a) where show x = genericShow x
derive instance Functor Link
derive instance Foldable Link
derive instance Traversable Link

instance Pretty (Link String) where
  pretty (IndentLink link) = indentString (pretty link)
  pretty (NewlineLink link) = "\n" <> pretty link
  pretty (CommentLink str link) = "/*" <> str <> "*/ " <> pretty link
  pretty (IdLink str) = str

--
-- Utilities
--

indentString :: String -> String 
indentString = Array.intercalate "\n" <<< map ("  " <> _) <<< String.split (Pattern "\n")

