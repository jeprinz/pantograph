module Pantograph.Library.Shallow where

import Pantograph.Generic.Language
import Prelude

import Data.HeteList (ConsTypeList, HeteList, NilTypeList, indHeteList)
import Data.Supertype (inject)
import Data.Symbol (class IsSymbol)
import Data.Tree (Change(..), Tree(..), injectTreeIntoChange)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row as R
import Record as Rec
import Type.Proxy (Proxy)

-- SortShallowSyntax

class SortShallowSyntax (sn :: Type) cs r | cs -> r sn where
  buildSortShallowSyntax :: Proxy sn -> HeteList cs -> Record r

instance SortShallowSyntax sn NilTypeList () where
  buildSortShallowSyntax _ _ = {}

instance
  ( IsSymbol x, R.Lacks x r
  , R.Cons x (a -> Sort sn) r r'
  , SortShallowSyntax sn cs r
  ) => SortShallowSyntax sn (ConsTypeList (Proxy x /\ (a -> Sort sn)) cs) r'
  where
  buildSortShallowSyntax p_sn = indHeteList \(p_x /\ f) cs ->
    let r = buildSortShallowSyntax p_sn cs in
    r # Rec.insert p_x f

-- RuleSortShallowSyntax

class RuleSortShallowSyntax (sn :: Type) cs r | cs -> r where
  buildRuleSortShallowSyntax :: Proxy sn -> HeteList cs -> Record r

instance RuleSortShallowSyntax sn NilTypeList () where
  buildRuleSortShallowSyntax _ _ = {}

instance
  ( IsSymbol x, R.Lacks x r
  , R.Cons x (a -> RuleSort sn) r r'
  , RuleSortShallowSyntax sn cs r
  ) => RuleSortShallowSyntax sn (ConsTypeList (Proxy x /\ (a -> Sort sn)) cs) r'
  where
  buildRuleSortShallowSyntax p_sn = indHeteList \(p_x /\ f) cs ->
    let r = buildRuleSortShallowSyntax p_sn cs in
    r # Rec.insert p_x (inject <<< f)

-- SortChangeShallowSyntax

class SortChangeShallowSyntax (sn :: Type) cs r | cs -> r where
  buildSortChangeShallowSyntax :: Proxy sn -> HeteList cs -> Record r

instance SortChangeShallowSyntax sn NilTypeList () where
  buildSortChangeShallowSyntax _ _ = {}

instance
  ( IsSymbol x, R.Lacks x r
  , R.Cons x (a -> SortChange sn) r r'
  , SortChangeShallowSyntax sn cs r
  ) => SortChangeShallowSyntax sn (ConsTypeList (Proxy x /\ (a -> Sort sn)) cs) r'
  where
  buildSortChangeShallowSyntax p_sn = indHeteList \(p_x /\ f) cs ->
    let r = buildSortChangeShallowSyntax p_sn cs in
    r # Rec.insert p_x (injectTreeIntoChange <<< f)

-- ExprShallowSyntax

class ExprShallowSyntax (sn :: Type) (er :: Row Type) cs r | sn -> er, cs -> r where
  buildExprShallowSyntax :: Proxy sn -> Proxy er -> HeteList cs -> Record r

instance ExprShallowSyntax sn er NilTypeList () where
  buildExprShallowSyntax _ _ _ = {}

instance
  ( IsSymbol x, R.Lacks x r
  , R.Cons x (a -> AnnExpr sn el er) r r'
  , ExprShallowSyntax sn er cs r
  ) => ExprShallowSyntax sn er (ConsTypeList (Proxy x /\ (a -> AnnExpr sn el er)) cs) r'
  where
  buildExprShallowSyntax p_sn p_er = indHeteList \(p_x /\ f) cs ->
    let r = buildExprShallowSyntax p_sn p_er cs in
    r # Rec.insert p_x f
