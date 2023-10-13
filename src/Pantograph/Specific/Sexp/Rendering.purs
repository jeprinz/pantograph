module Pantograph.Specific.Sexp.Rendering where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Pantograph.Specific.Sexp.Language
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
import Pantograph.Generic.Rendering as PGR
import Partial.Unsafe (unsafePartial)
import Record as R
import Text.Pretty (class Pretty, parens, pretty, (<+>))
import Text.Pretty as Pretty
import Type.Proxy (Proxy(..))

type RenderCtx = ()

type RenderEnv = ()

type Renderer = PGR.Renderer SN EL RenderCtx RenderEnv

renderer :: Renderer
renderer = PGR.Renderer
  { name: "basic"
  , language
  , topCtx: {}
  , topEnv: {}
  , arrangeExpr:
      \node@(PGL.AnnExprNode {label}) ->
        let punc str = PGR.HtmlArrangeKid [HH.span_ [HH.text str]] in
        let msg = "arrangeExpr" <+> "{" <> "label:" <+> show label <> "}" in
        let ass = assertValidTreeKids msg node in
        case label of
          StringRule str -> ass \[] -> pure [PGR.HtmlArrangeKid [HH.span [HP.classes [HH.ClassName "string"]] [HH.text str]]]
          VarRule -> ass \[mx] -> do
            x /\ _ <- mx
            pure [punc "#", PGR.ExprKidArrangeKid x]
          AppRule -> ass \[mf, ma] -> do
            f /\ _ <- mf
            a /\ _ <- ma
            pure [punc "(", PGR.ExprKidArrangeKid f, punc " ", PGR.ExprKidArrangeKid a, punc ")"]
  , beginsLine: \_ -> false
  }
