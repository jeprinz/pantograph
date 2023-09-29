module Pantograph.Generic.Editor.Common where

import Pantograph.Generic.Language
import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Data.Fix (Fix)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Halogen as H
import Halogen as HH
import Halogen.Hooks as HK
import Halogen.Utilities as HU

-- # Pantograph.Generic.Editor.Common

-- ## Editor

class 
  ( Language rule joint tooth
  , ToClassNames rule ) 
  <= Editor rule joint tooth 
  where
  arrangeExpr :: forall ctx env.
    PrgmExprJoint rule joint joint (ExprHtml rule joint tooth) ->
    RenderM ctx env rule joint tooth (Array (EditorHtml rule joint tooth))

class ToClassNames a where
  toClassNames :: a -> Array HH.ClassName

-- ## RenderM

type RenderM ctx env rule joint tooth = 
  ReaderT (RenderCtx ctx env rule joint tooth) (
  StateT (RenderEnv ctx env rule joint tooth) Identity
  )

runRenderM :: forall ctx env rule joint tooth a. 
  RenderCtx ctx env rule joint tooth ->
  RenderEnv ctx env rule joint tooth ->
  RenderM ctx env rule joint tooth a ->
  a
runRenderM ctx env = flip runReaderT ctx >>> flip evalStateT env >>> unwrap

-- ### RenderCtx

type RenderCtx (ctx :: Row Type) (env :: Row Type) rule joint tooth =
  { depth :: Int
  , bufferId :: HK.StateId (Buffer rule joint tooth)
  , outputToken :: HK.OutputToken (BufferOutput rule joint tooth)
  | ctx }

-- ### RenderCtx

type RenderEnv (ctx :: Row Type) (env :: Row Type) (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type) =
  { holeCount :: Int
  | env }

-- ## RenderExprHtml, ExprHtml

type RenderExprHtml ctx env rule joint tooth = 
  RenderM ctx env rule joint tooth (ExprHtml rule joint tooth)

type ExprHtml rule joint tooth =
  { htmlExpr :: Fix (HtmlExprJoint rule joint)
  , html :: EditorHtml rule joint tooth
  }

-- type RenderedExpr rule joint = Fix (RenderedExpr (ExprJoint rule (Sort sortJoint) (OpenJoint exprJoint)))

type HtmlExprJoint rule joint = HtmlJoint (PrgmExprJoint rule joint joint)

data HtmlJoint (joint :: Type -> Type) a
  = Html HU.ElementId (joint a)

-- ## EditorHtml

type EditorHtml rule joint tooth = HH.ComponentHTML (HK.HookM Aff Unit) (EditorSlots rule joint tooth) Aff

type EditorSlots rule joint tooth =
  ( editor :: H.Slot (EditorQuery rule joint tooth) (EditorOutput rule joint tooth) EditorSlot
  , buffer :: H.Slot (BufferQuery rule joint tooth) (BufferOutput rule joint tooth) BufferSlot
  , toolbox :: H.Slot (ToolboxQuery rule joint tooth) (ToolboxOutput rule joint tooth) ToolboxSlot
  , preview :: H.Slot (PreviewQuery rule joint tooth) (PreviewOutput rule joint tooth) PreviewSlot
  , clipboard :: H.Slot (ClipboardQuery rule joint tooth) (ClipboardOutput rule joint tooth) ClipboardSlot
  , console :: H.Slot (ConsoleQuery rule joint tooth) (ConsoleOutput rule joint tooth) ConsoleSlot )

-- ## Components

-- ### Editor

data EditorSlot
data EditorQuery (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type) (a :: Type)
data EditorInput (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)
data EditorOutput (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)

-- ### Buffer

data BufferSlot
data BufferQuery (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type) (a :: Type)
newtype BufferInput (ctx :: Row Type) (env :: Row Type) (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type) = BufferInput
  { ctx :: Record ctx
  , env :: Record env
  , buffer :: Buffer rule joint tooth }
data BufferOutput (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)

data Buffer (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)
  = TopBuffer (Fix (PrgmExprJoint rule joint joint))
  | CursorBuffer (Fix (PrgmCursorJoint rule joint joint tooth))
  | SelectBuffer (Fix (PrgmSelectJoint rule joint joint tooth))

-- ### Toolbox

data ToolboxSlot
data ToolboxQuery (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type) (a :: Type)
data ToolboxInput (ctx :: Row Type) (env :: Row Type) (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)
data ToolboxOutput (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)

-- ### Preview

data PreviewSlot
data PreviewQuery (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type) (a :: Type)
data PreviewInput (ctx :: Row Type) (env :: Row Type) (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)
data PreviewOutput (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)

-- ### Clipboard

data ClipboardSlot
data ClipboardQuery (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type) (a :: Type)
data ClipboardInput (ctx :: Row Type) (env :: Row Type) (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)
data ClipboardOutput (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)

-- ### Console

data ConsoleSlot
data ConsoleQuery (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type) (a :: Type)
data ConsoleInput (ctx :: Row Type) (env :: Row Type) (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)
data ConsoleOutput (rule :: Type) (joint :: Type -> Type) (tooth :: Type -> Type)

