module Text.Pretty where

import Prelude

import Data.Array (concat, foldMap, intercalate)
import Data.Functor.Compose (Compose(..))
import Data.List as List
import Data.List.Rev as Rev
import Data.Maybe (Maybe, maybe)
import Data.String (Pattern(..))
import Data.String as String

class Pretty a where pretty :: a -> String

instance Pretty String where pretty = identity
instance Pretty Int where pretty = show
instance Pretty Boolean where pretty = show
instance Pretty a => Pretty (List.List a) where pretty xs = "[" <> List.intercalate ", " (pretty <$> xs) <> "]"
instance Pretty a => Pretty (Array a) where pretty xs = "[" <> intercalate ", " (pretty <$> xs) <> "]"
instance Pretty a => Pretty (Maybe a) where pretty = maybe "NOTHING" pretty

-- | Pretty `a` that takes an argument of type `b`.
class Pretty' a b | a -> b where
  pretty' :: b -> a -> String

instance Pretty a => Pretty' (List.List a) String where pretty' sep xs = "[" <> List.intercalate sep (pretty <$> xs) <> "]"
instance Pretty a => Pretty' (Rev.List a) String where pretty' sep xs = "[" <> List.intercalate sep (pretty <$> Rev.unreverse xs) <> "]"
instance Pretty a => Pretty' (Array a) String where pretty' sep xs = "[" <> intercalate sep (pretty <$> xs) <> "]"
instance Pretty a => Pretty' (Maybe a) String where pretty' nothing = maybe nothing pretty

-- class PrettyContainer t where prettyContainer :: forall a. Pretty a => t a -> String

-- instance PrettyContainer List.List where prettyContainer xs = "[" <> List.foldMap pretty xs <> "]"
-- instance PrettyContainer Array where prettyContainer xs = "[" <> foldMap pretty xs <> "]"
-- instance PrettyContainer Maybe where prettyContainer xs = maybe "NOTHING" pretty xs

-- instance (PrettyContainer t1, Functor t1, PrettyContainer t2) => PrettyContainer (Compose t1 t2) where prettyContainer (Compose t12) = prettyContainer (prettyContainer <$> t12)

-- -- instance (PrettyContainer t, Functor t, Pretty a) => Pretty (t a) where pretty ta = prettyContainer (pretty <$> ta)

appendSpaced :: String -> String -> String
appendSpaced str1 str2 | String.null str1 = str2
appendSpaced str1 str2 | String.null str2 = str1
appendSpaced str1 str2 = str1 <> " " <> str2

infixr 5 appendSpaced as <+>

indent :: String -> String
indent = intercalate "\n" <<< map ("  " <> _) <<< String.split (Pattern "\n")
