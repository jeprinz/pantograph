module Pantograph.Specific.Sexp.Language where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Control.Monad.Reader (ask, local)
import Control.Monad.State as State
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (curry)
import Debug as Debug
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hole (hole)
import Pantograph.Generic.Language as PGL
import Partial.Unsafe (unsafePartial)
import Record as R
import Text.Pretty (class Pretty, parens, pretty, (<+>))
import Text.Pretty as Pretty
import Type.Proxy (Proxy(..))

data EL
  = StringRule String
  | AppRule
  | VarRule
derive instance Generic EL _
derive instance Eq EL
derive instance Ord EL
instance Show EL where show = genericShow

instance TreeNode EL where
  kidsCount = case _ of
    StringRule _ -> 0
    AppRule -> 2
    VarRule -> 1

instance PrettyTreeNode EL where
  prettyTreeNode el = case el of
    StringRule str -> assertValidTreeKids "prettyTreeNode" el \[] -> str
    AppRule -> assertValidTreeKids "prettyTreeNode" el \[f, a] -> parens (f <+> a)
    VarRule -> assertValidTreeKids "prettyTreeNode" el \[x] -> "#" <> x

data SN 
  = StringSort
  | SexpSort
derive instance Generic SN _
derive instance Eq SN
derive instance Ord SN
instance Show SN where show = genericShow

type RenderCtx = ()

type RenderEnv = ()

type Expr = PGL.Expr SN EL
type RuleSort = PGL.RuleSort SN
type Sort = PGL.Sort SN
type Language = PGL.Language SN EL

language :: Language
language = PGL.Language
  { name: "s-expressions"
  , getSortingRule: case _ of
      StringRule _ -> PGL.SortingRule
        { parameters: Set.empty
        , kids: [] 
        , parent: string.ruleSort }
      AppRule -> PGL.SortingRule
        { parameters: Set.empty
        , kids: [] 
        , parent: sexp.ruleSort }
      VarRule -> PGL.SortingRule
        { parameters: Set.empty
        , kids: [] 
        , parent: sexp.ruleSort }
  , getChangingRule: case _ of
      StringRule _ -> PGL.ChangingRule 
        { parameters: Set.empty 
        , kids: [] }
      AppRule -> PGL.ChangingRule
        { parameters: Set.empty
        , kids: [Reflect (PGL.ConstRuleSortNode (PGL.SortNode SexpSort)) []] }
      VarRule -> PGL.ChangingRule
        { parameters: Set.empty
        , kids: [] }
  , topSort: sexp.sort
  , defaultExpr: case _ of
      Tree {node: PGL.SortNode StringSort, kids: []} -> Nothing
      Tree {node: PGL.SortNode SexpSort, kids: []} ->
        -- Just $ sexp.var (string.string "x")
        -- Just $ sexp.example 4
        Just $ sexp.app "x1" $ sexp.app "x2" $ sexp.var "x3"
      _ -> bug "[defaultExpr] invalid sort"
  }

sexp = {string, app, var, ruleSort, sort, example}
  where
  var str = PGL.makeExpr VarRule [] [string.string str]

  app str a = PGL.makeExpr AppRule [] [string.string str, a]

  example 0 = var "y0"
  example n = app ("x" <> show n) (app ("f" <> show n) (example (n - 1)))

  ruleSort :: RuleSort
  ruleSort = PGL.makeConstRuleSort SexpSort []

  sort :: Sort
  sort = PGL.makeSort SexpSort []

string = {string, ruleSort, sort}
  where
  string str = PGL.makeExpr (StringRule str) [] []

  ruleSort :: RuleSort
  ruleSort = PGL.makeConstRuleSort StringSort []

  sort :: Sort
  sort = PGL.makeSort StringSort []