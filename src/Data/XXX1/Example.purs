module Data.XXX1.Example where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (class Traversable)
import Data.XXX1 as Gram
import Partial.Unsafe (unsafeCrashWith)

--
-- Proto
--

data Proto var 
  = Use var
  | Lam var
  | App

derive instance Functor Proto
derive instance Foldable Proto
derive instance Traversable Proto

--
-- Expr
--

type Expr = Gram.Expr Proto Link Var
type ExprFix rec = Gram.ExprFix Proto Link Var rec

useExpr :: Var -> Expr
useExpr var = Gram.Gram {proto: Use var, joint: Gram.ExprJoint []}

lamExpr :: Var -> Expr -> Expr
lamExpr var bod = Gram.Gram {proto: Use var, joint: Gram.ExprJoint $ IdLink <$> [bod]}

appExpr :: Expr -> Expr -> Expr
appExpr apl arg = Gram.Gram {proto: App, joint: Gram.ExprJoint $ IdLink <$> [apl, arg]}

matchExpr :: forall a rec.
  { use :: {var :: Var} -> a
  , lam :: {var :: Var, bod :: Link rec} -> a
  , app :: {apl :: Link rec, arg :: Link rec} -> a
  } ->
  ExprFix rec -> a
matchExpr f {proto: Use var, joint: Gram.ExprJoint []} = f.use {var}
matchExpr f {proto: Lam var, joint: Gram.ExprJoint [ bod ]} = f.lam {var, bod}
matchExpr f {proto: App, joint: Gram.ExprJoint [ apl, arg ]} = f.app {apl, arg}
matchExpr _ _ = unsafeCrashWith "matchExpr: invalid Expr"

showExpr :: Expr -> String
showExpr = Gram.foldMapGram $ matchExpr  
  { use: \{var} -> show var
  , lam: \{var, bod} -> "(λ " <> show var <> " ⇒ " <> show bod <> ")"
  , app: \{apl, arg} -> "(" <> show apl <> show arg <> ")"
  }

--
-- Path
--

type Path = Gram.Path Proto Link Var

--
-- Change
--

type Change = Gram.Change Proto Link Var Metavar

--
-- Var
--

data Var = Var String

instance Show Var where
  show (Var str) = str

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

derive instance Functor Link
derive instance Foldable Link
derive instance Traversable Link

instance Show (Link String) where
  show (IndentLink link) = indentString (show link)
  show (NewlineLink link) = "\n" <> show link
  show (CommentLink str link) = "/*" <> str <> "*/ " <> show link
  show (IdLink str) = str

--
-- Utilities
--

indentString :: String -> String 
indentString = Array.intercalate "\n" <<< map ("  " <> _) <<< String.split (Pattern "\n")

