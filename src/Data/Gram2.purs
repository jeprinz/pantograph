module Data.Gram2 where

{-
import Prelude
import Text.Pretty ((<+>))
import Data.Bifunctor (bimap)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Functor.Compose (Compose)
import Data.Identity (Identity(..))
import Data.List.Zip (ToothH(..), showPath, showToothH)
import Data.List.Zip as Zip
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (class Foldable, class Traversable, foldMap, intercalate)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Utility (map3, (<$$$>))

-- | Gram
newtype Gram :: Type → (Type → Type) → (Type → Type) → Type
newtype Gram l j1 j2 = Gram (Gram' l j1 j2 (Gram l j1 j2))
type Gram' l j1 j2 gram = j1 (l /\ j2 gram)

derive instance Newtype (Gram l j1 j2) _

newtype MetaVar = MetaVar String

derive newtype instance Show MetaVar

showMetaVar :: MetaVar -> String
showMetaVar (MetaVar str) = str

-- mapGram :: forall l j1 j2 m. Functor j1 => Functor j2 => Applicative m => 
--   (j1 (Tuple l (j2 (m (Gram l j1 j2)))) -> j1 (Tuple l (j2 (Gram l j1 j2)))) -> 
--   Gram l j1 j2 -> m (Gram l j1 j2)
mapGram f = traverseGram (pure <<< f)

-- foldMapGram :: forall l j j2 m. 
--   Functor j => Functor j2 => Monoid m => 
--   (j (Tuple l (j2 m)) -> m) -> 
--   Gram l j j2 -> m
foldMapGram f = unwrap <<< traverseGram (Const <<< f <<< map3 unwrap)

traverseGram ∷ ∀ l l' j1 j2 m. 
  Functor j1 ⇒ Functor j2 ⇒ Applicative m ⇒ 
  (Gram' l j1 j2 (m (Gram l' j1 j2)) → m (Gram' l' j1 j2 (Gram l' j1 j2))) →
  Gram l j1 j2 → m (Gram l' j1 j2)
traverseGram f (Gram g') = Gram <$> f (traverseGram f <$$$> g')

showGram = foldMapGram (foldMap \(l /\ jnt) -> "(" <> show l <> " " <> intercalate " " jnt <> ")")

-- Path (up or down, depending on your interpretation)
type Path l step = Gram l step (Zip.ToothH (Expr l))
type PathUp l = Path l StepUp
type PathDown l = Path l StepDown

newtype StepUp path = StepUp (Maybe path)
newtype StepDown path = StepDown (Maybe path)

derive instance Newtype (StepUp path) _
derive instance Newtype (StepDown path) _

-- p1 goes above
appendPathUp :: forall l. PathUp l -> PathUp l -> PathUp l
appendPathUp p1 = unwrap >>> unwrap >>> maybe p1 \(l /\ ZipH th) -> Gram $ StepUp $ Just $ l /\ ZipH {path: th.path, focus: appendPathUp p1 th.focus}

-- p1 goes below
appendPathDown :: forall l. PathDown l -> PathDown l -> PathDown l
appendPathDown p1 = unwrap >>> unwrap >>> maybe p1 \(l /\ ZipH th) -> Gram $ StepDown $ Just $ l /\ ZipH {path: th.path, focus: appendPathDown p1 th.focus}

-- Expression
type GenExpr l f = Gram l f Array
type Expr l = GenExpr l Identity
type MetaExpr l = GenExpr l (Either MetaVar)

showExpr :: forall l f. Show l => Functor f => Foldable f => GenExpr l f -> String
showExpr = foldMapGram $ foldMap case _ of 
  l /\ kids -> "(" <> show l <+> intercalate " " kids <> ")"

-- Change
type Change l = Gram l (Either MetaVar) (ChangeJoint l)

data ChangeJoint l change
  = Plus (Zip.ToothH (Expr l) change)
  | Minus (Zip.ToothH (Expr l) change)
  | Expr (Array change)
  | Replace (Expr l /\ Expr l)

derive instance Functor (ChangeJoint l)
derive instance Foldable (ChangeJoint l)
derive instance Traversable (ChangeJoint l)

-- showChange :: forall l. Show l => Change l -> String
-- showChange = foldMapGram case _ of
--   Left mv -> show mv 
--   Right (l /\ (Plus th)) -> "(+ " <> show l <+> showToothH (bimap showExpr identity th) <> ")"
--   Right (l /\ (Minus th)) -> "(- " <> show l <+> showToothH (bimap showExpr identity th) <> ")"
--   Right (l /\ (Expr kids)) -> "(" <> show l <+> intercalate " " kids <> ")"
--   Right (l /\ (Replace (e1 /\ e2))) -> ?a

ch :: Change Int
ch = Gram $ Right $ 1 /\ Replace ((Gram $ Identity $ 1 /\ []) /\ (Gram $ Identity $ 2 /\ []))
-}