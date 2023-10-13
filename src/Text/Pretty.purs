module Text.Pretty where

import Prelude

import Data.Array (concat, foldMap, intercalate)
import Data.Functor.Compose (Compose(..))
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe, maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))

--------------------------------------------------------------------------------
-- Pretty
--------------------------------------------------------------------------------

class Pretty a where pretty :: a -> String

instance Pretty String where pretty = identity
instance Pretty Int where pretty = show
instance Pretty Boolean where pretty = show
instance Pretty a => Pretty (List.List a) where pretty xs = "[" <> List.intercalate ", " (pretty <$> xs) <> "]"
instance Pretty a => Pretty (Array a) where pretty xs = "[" <> intercalate ", " (pretty <$> xs) <> "]"
instance Pretty a => Pretty (Maybe a) where pretty = maybe "NOTHING" pretty
instance (Pretty a, Pretty b) => Pretty (Tuple a b) where pretty (Tuple a b) = pretty a <> ", " <> pretty b
instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where 
  pretty m =
    "map:" <>
    indent (bullets (Map.toUnfoldable m <#> \(Tuple k v) -> pretty k <> " ↦ " <> pretty v))
instance (Pretty t) => Pretty (Set.Set t) where
  pretty s = "{" <> List.intercalate ", " (Set.map pretty s) <> "}"

appendSpaced :: String -> String -> String
appendSpaced str1 str2 | String.null str1 = str2
appendSpaced str1 str2 | String.null str2 = str1
appendSpaced str1 str2 = str1 <> " " <> str2

infixr 5 appendSpaced as <+>

cursor :: String
cursor = "⌶"

indent :: String -> String
indent = intercalate "\n" <<< map ("  " <> _) <<< String.split (Pattern "\n")

surround :: String -> String -> String -> String
surround left right str = left <> str <> right

quotes   = surround "'" "'"
quotes2  = surround "\"" "\""
parens   = surround "(" ")"
brackets = surround "[" "]"
braces   = surround "{" "}"
braces2   = surround " {{ " " }} "
angles   = surround "<" ">"
ticks    = surround "`" "`"

outer = surround " <{ " " }> "
inner = surround " {> " " <} "

outerActive = surround " <*{ " " }*> "
innerActive = surround " {*> " " <*} "

spaces = intercalate " "
commas = intercalate ", "
newlines = intercalate "\n"
-- bullets = intercalate "\n  - "
bullets :: Array String -> String
bullets = indent <<< foldMap ("\n- " <> _) <<< map (intercalate "\n  " <<< String.split (Pattern "\n"))

--------------------------------------------------------------------------------
-- Pretty1
--------------------------------------------------------------------------------

class Pretty1 t where
  pretty1 :: forall a. Pretty a => t a -> String

instance Pretty1 List.List where pretty1 = pretty
instance Pretty1 Array where pretty1 = pretty
instance Pretty1 Maybe where pretty1 = pretty
instance Pretty a => Pretty1 (Tuple a) where pretty1 = pretty
instance Pretty k => Pretty1 (Map.Map k) where pretty1 = pretty
instance Pretty1 Set.Set where pretty1 = pretty

--------------------------------------------------------------------------------
-- PrettyS
--------------------------------------------------------------------------------

class PrettyS a where
  prettyS :: a -> String -> String

class PrettyS1 (t :: Type -> Type) where
  prettyS1 :: forall a. Pretty a => t a -> String -> String