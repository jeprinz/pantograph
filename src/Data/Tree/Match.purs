module Data.Tree.Match
  -- ( class UnionTreePatternHeteListRows
  -- , TreePattern, treePattern, varTreePattern, indTreePattern
  -- )
  where

import Data.HeteList
import Data.Tree
import Prelude
import Prim.Row

import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Record as R
import Record.Unsafe.Union as RU
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- GetPatternVars

class GetPatternVars (a :: Type) (vars :: Row Type) | a -> vars

-- UnionPatternVars

class UnionHeteListPatternVars (kids :: Type) (vars :: Row Type) | kids -> vars
instance UnionHeteListPatternVars NilTypeList ()
instance (UnionHeteListPatternVars kids vars1, GetPatternVars a vars2, Union vars1 vars2 vars) => UnionHeteListPatternVars (ConsTypeList a kids) vars

-- TreePattern

data TreePattern (a :: Type) (kids :: Type) (vars :: Row Type)
  = TreePattern a (HeteList kids)
  | VarTreePattern (forall (x :: Symbol). Proxy x)

instance GetPatternVars (TreePattern a kids vars) vars

treePattern :: forall a kids vars. 
  UnionHeteListPatternVars kids vars => 
  a -> HeteList kids -> TreePattern a kids vars
treePattern = unsafeCoerce unit

varTreePattern :: forall a (x :: Symbol) vars. Cons x (Tree a) () vars => Proxy x -> TreePattern a NilTypeList vars
varTreePattern px = unsafeCoerce VarTreePattern px

indTreePattern :: forall a _kids vars b.
  { tree :: forall kids. UnionHeteListPatternVars kids vars => a -> HeteList kids -> b
  , var  :: forall (x :: Symbol). IsSymbol x => Cons x (Tree a) () vars => Proxy x -> b } ->
  TreePattern a _kids vars -> b
indTreePattern ind = case _ of
  TreePattern a kids -> unsafeCoerce ind.tree a kids
  VarTreePattern px -> unsafeCoerce ind.var px

matchTreePattern :: forall a kids vars. Eq a => TreePattern a kids vars -> Tree a -> Maybe (Record vars)
matchTreePattern p t = p # indTreePattern
  { tree: \a kids -> case t of
      Tree a' kids' -> if a /= a' then Nothing else foldlCoerce (unsafeCoerce \kid -> maybe Nothing \(kid' /\ vars) -> RU.unsafeUnion vars <$> matchTreePattern kid (kid' :: Tree a)) (Just (kids' /\ {})) kids
  , var: \px -> Just $ R.insert px t {}
  }

-- -- examples
-- tr1 = varTreePattern (Proxy :: Proxy "x")
-- tr2 = treePattern "a" (varTreePattern (Proxy :: Proxy "x") : varTreePattern (Proxy :: Proxy "y") : varTreePattern (Proxy :: Proxy "z") : nil)

-- xxx = matchTreePattern tr2 (Tree "a" [Tree "b" [], Tree "c" [], Tree "d" []]) >>= \{x, y, z} -> pure unit

-- ToothPattern

data ToothPattern (a :: Type) (kids :: Type) (vars :: Row Type)
  = ToothPattern a Int (HeteList kids)
  | VarToothPattern (forall (x :: Symbol). Proxy x)

instance GetPatternVars (ToothPattern a kids vars) vars

toothPattern :: forall a kids vars. 
  UnionHeteListPatternVars kids vars => 
  a -> HeteList kids -> ToothPattern a kids vars
toothPattern = unsafeCoerce unit

varToothPattern :: forall a (x :: Symbol) vars. Cons x (Tooth a) () vars => Proxy x -> ToothPattern a NilTypeList vars
varToothPattern px = unsafeCoerce VarToothPattern px

indToothPattern :: forall a _kids _vars b.
  { tooth :: forall kids vars. UnionHeteListPatternVars kids vars => a -> Int -> HeteList kids -> b
  , var  :: forall x. Proxy x -> b } ->
  ToothPattern a _kids _vars -> b
indToothPattern ind = case _ of
  ToothPattern a i kids -> unsafeCoerce ind.tooth a i kids
  VarToothPattern px -> unsafeCoerce ind.var px

-- -- examples
-- th1 = varToothPattern (Proxy :: Proxy "x")
-- th2 = toothPattern "a" (varTreePattern (Proxy :: Proxy "x") : varTreePattern (Proxy :: Proxy "y") : varTreePattern (Proxy :: Proxy "z") : nil)

-- ChangePattern

data ChangePattern (a :: Type) (kids :: Type) (vars :: Row Type)
  = InjectChangePattern a (HeteList kids)
  | Shift (ShiftSign /\ ToothPattern a kids vars) (ChangePattern a kids vars)
  | Replace (TreePattern a kids vars) (TreePattern a kids vars)
  | VarChangePattern (forall (x :: Symbol). Proxy x)

instance GetPatternVars (ChangePattern a kids vars) vars

injectChangePattern :: forall a kids vars.
  UnionHeteListPatternVars kids vars =>
  a -> HeteList kids -> ChangePattern a kids vars
injectChangePattern a kids = unsafeCoerce InjectChangePattern a kids  

shiftChangePattern :: forall a kids1 vars1 kids2 vars2 vars.
  Union vars1 vars2 vars =>
  ShiftSign /\ ToothPattern a kids1 vars1 ->
  ChangePattern a kids2 vars2 ->
  ChangePattern a NilTypeList vars
shiftChangePattern (sh /\ th) ch = unsafeCoerce Shift (sh /\ th) ch

replaceChangePattern :: forall a kids1 vars1 kids2 vars2 vars.
  Union vars1 vars2 vars =>
  TreePattern a kids1 vars1 ->
  TreePattern a kids2 vars2 ->
  ChangePattern a NilTypeList vars
replaceChangePattern t1 t2 = unsafeCoerce Replace t1 t2

varChangePattern :: forall a x vars.
  Cons x (Change a) () vars =>
  Proxy x ->
  ChangePattern a NilTypeList vars
varChangePattern px = unsafeCoerce VarChangePattern px

indChangePattern :: forall a _kids _vars b.
  { inject :: forall kids vars. UnionHeteListPatternVars kids vars => a -> HeteList kids -> b 
  , shift :: forall kids1 vars1 kids2 vars2 vars. Union vars1 vars2 vars => (ShiftSign /\ ToothPattern a kids1 vars1) -> ChangePattern a kids2 vars2 -> b
  , replace :: forall kids1 vars1 kids2 vars2 vars. Union vars1 vars2 vars => TreePattern a kids1 vars1 -> TreePattern a kids2 vars2 -> b
  , var :: forall x. Proxy x -> b } ->
  ChangePattern a _kids _vars -> b
indChangePattern ind = case _ of
  InjectChangePattern a kids -> unsafeCoerce ind.inject a kids
  Shift (sh /\ th) ch -> unsafeCoerce ind.shift (sh /\ th) ch
  Replace t1 t2 -> unsafeCoerce ind.replace t1 t2
  VarChangePattern px -> unsafeCoerce ind.var px

-- -- examples
-- ch1 = varChangePattern (Proxy :: Proxy "ch1")
-- ch2 = injectChangePattern "a" 
--   ( replaceChangePattern
--       (varTreePattern (Proxy :: Proxy "t1"))
--       (varTreePattern (Proxy :: Proxy "t2"))
--   : varChangePattern (Proxy :: Proxy "ch1")
--   : nil)
