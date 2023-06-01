module Language.Pantograph.Generic.Rendering.Base where

import Language.Pantograph.Generic.Edit
import Language.Pantograph.Generic.Grammar
import Prelude
import Type.Direction

import Bug.Assertion (Assertion(..), assert, just)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either)
import Data.Either.Nested (type (\/))
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Expr (class ReflectPathDir)
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy)
import Data.List.Zip as ZipList
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (case_, on)
import Data.Zippable (class Zippable)
import Data.Zippable as Zippable
import Effect.Aff (Aff, throwError)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML (ComponentHTML) as HH
import Halogen.Hooks as HK
import Language.Pantograph.Generic.Unification (Sub)
import Text.Pretty (class Pretty, pretty)
import Text.Pretty as P
import Type.Proxy (Proxy(..))

type EditorHTML l r = 
  HH.ComponentHTML 
    (HK.HookM Aff Unit)
    ( buffer :: H.Slot Query (Output l r) Unit
    , preview :: H.Slot (PreviewQuery l r) Unit HorizontalDir
    ) 
    Aff

data EditPreviewHTML l r
  = FillEditPreview (EditorHTML l r)
  | ReplaceEditPreview (EditorHTML l r)
  | WrapEditPreview {before :: Array (EditorHTML l r), after :: Array (EditorHTML l r)}

type RenderingContext = 
  { indentationLevel :: Int
  , isCursor :: Boolean
  , isInteractive :: Boolean
  , isInlined :: Boolean
  }

incremementIndentationLevel :: RenderingContext -> RenderingContext
incremementIndentationLevel ctx = ctx {indentationLevel = ctx.indentationLevel + 1}

data Linebreak
  = IndentedLinebreak
  | UnindentedLinebreak

defaultRenderingContext :: RenderingContext
defaultRenderingContext = 
  { indentationLevel: 0
  , isCursor: false
  , isInteractive: true
  , isInlined: false
  }

previewRenderingContext :: RenderingContext
previewRenderingContext = 
  { indentationLevel: 0
  , isCursor: false
  , isInteractive: false
  , isInlined: true
  }

type ArrangeDerivTermSubs l r = 
  { renCtx :: RenderingContext
  , rule :: r
  , sort :: Sort l
  , kids :: Array (DerivTerm l r)
  } -> 
  Array (PreKid l r)

type PreKid l r = 
  (RenderingContext /\ Int) \/ -- reference to kid, with kid's rendering context
  Array (EditorHTML l r) -- static html

-- !TODO editor spec shouldn't know about HoleyDerivZipper, so need two
-- "editsAt..." types
type EditorSpec l r =
  { hdzipper :: HoleyDerivZipper l r
  , topSort :: Sort l
  , editsAtHoleyDerivZipper :: Sort l -> HoleyDerivZipper l r -> Array (Edit l r)
  
  -- -- the output terms are valid (already checked via unification when generated)
  -- !TODO editsAtHoleInterior :: Sort l -> Array (HoleInteriorEdit l r)
  
  -- -- corresponds to a change where the path is inserted and the topChange is
  -- -- inserted as a boundary at the top, and likewise for botChange, and them
  -- -- smallstep figured out the final result
  -- !TODO editsAtCursor :: Sort l -> Array (CursorEdit l r)

  -- !TODO isValidCursorSort :: Grammar.Sort l -> Boolean
  -- !TODO isValidSelectionSorts :: Grammar.Sort l -> Grammar.Sort l -> Boolean
  
  , arrangeDerivTermSubs :: ArrangeDerivTermSubs l r
  }

-- Stuff that's defined inside of the editor component
type EditorLocals l r = 
  { clipboard_ref :: Ref.Ref (Maybe (Either (DerivPath Up l r) (DerivTerm l r)))
  , currentState :: State l r
  , facade_ref :: Ref.Ref (State l r)
  , initState :: State l r
  , input :: EditorSpec l r
  , maybeHighlightPath_ref :: Ref.Ref (Maybe (HoleyDerivPath Up l r))
  , state_id :: HK.StateId (State l r)
  }

data State l r
  = CursorState (Cursor l r)
  | SelectState (Select l r)
  | TopState (Top l r)

derive instance Generic (State l r) _
instance (Show l, Show r) => Show (State l r) where show x = genericShow x

instance (Expr.IsExprLabel l, IsRuleLabel l r) => Pretty (State l r) where
  pretty = case _ of
    CursorState cursor -> Array.intercalate "\n"
      [ "cursor:"
      , P.indent $ P.newlines
          [ "- hdzipper = " <> pretty cursor.hdzipper
          , "- mode = " <> show cursor.mode
          ]
      ]
    SelectState select -> Array.intercalate "\n"
      [ "select:"
      , P.indent $ P.newlines
          [ "- dzipperp = " <> pretty select.dzipperp ]
      ]
    TopState _top -> Array.intercalate "\n"
      [ "top:"
      ]

type Cursor l r =
  { hdzipper :: HoleyDerivZipper l r
  , mode :: CursorMode
  }

data CursorMode 
  = NavigationCursorMode
  | BufferCursorMode

cursorState :: forall l r. String -> State l r -> Assertion (Cursor l r)
cursorState source st = Assertion
  { name: "cursorState"
  , source
  , result: case st of
      CursorState cursor -> pure cursor
      _ -> throwError "expected to be cursor state"
  }

derive instance Generic CursorMode _
instance Show CursorMode where show x = genericShow x

cursorFromHoleyDerivZipper :: forall l r. HoleyDerivZipper l r -> Cursor l r
cursorFromHoleyDerivZipper hdzipper = 
  { hdzipper
  , mode: NavigationCursorMode
  }

type Select l r =
  { dzipperp :: DerivZipperp l r
  }

type Top l r =
  { dterm :: DerivTerm l r
  }

data HoleyDerivZipper l r
  = InjectHoleyDerivZipper (DerivZipper l r)
  | HoleInteriorHoleyDerivZipper 
      (DerivPath Up l r) -- the path to the Hole
      (Sort l) -- the sort of the Hole

derive instance Generic (HoleyDerivZipper l r) _
derive instance (Eq l, Eq r) => Eq (HoleyDerivZipper l r)
derive instance (Ord l, Ord r) => Ord (HoleyDerivZipper l r)
instance (Show l, Show r) => Show (HoleyDerivZipper l r) where show x = genericShow x

instance IsRuleLabel l r => Pretty (HoleyDerivZipper l r) where
  pretty (InjectHoleyDerivZipper dzipper) = pretty dzipper
  pretty (HoleInteriorHoleyDerivZipper dpath sort) = Expr.prettyPath dpath $ "(⌶{?} : " <> pretty sort <> ")"

instance IsRuleLabel l r => Zippable (HoleyDerivZipper l r) where
  zipDowns (InjectHoleyDerivZipper dz) | Just sort <- isHoleDerivTerm (Expr.zipperExpr dz) = do
    [HoleInteriorHoleyDerivZipper (Expr.zipperPath dz) sort]
  zipDowns (InjectHoleyDerivZipper dz) = InjectHoleyDerivZipper <$> Zippable.zipDowns dz
  zipDowns (HoleInteriorHoleyDerivZipper _ _) = []
  zipUp' (InjectHoleyDerivZipper dz) = bimap identity InjectHoleyDerivZipper <$> Zippable.zipUp' dz
  zipUp' (HoleInteriorHoleyDerivZipper dpath sort) = 
    assert (just "Zippable (HoleyDerivZipper l r) . zipUp'" (defaultDerivTerm sort)) \dterm ->
      pure $ 0 /\ InjectHoleyDerivZipper (Expr.Zipper dpath dterm)

data HoleyDerivPath dir l r
  = InjectHoleyDerivPath (DerivPath dir l r)
  | HoleInteriorHoleyDerivPath (DerivPath Up l r) -- the path to the Hole

derive instance Generic (HoleyDerivPath dir l r) _
derive instance (Eq l, Eq r) => Eq (HoleyDerivPath dir l r)
derive instance (Ord l, Ord r) => Ord (HoleyDerivPath dir l r)
instance (Show l, Show r) => Show (HoleyDerivPath dir l r) where show x = genericShow x

instance (IsRuleLabel l r, ReflectPathDir dir) => Pretty (HoleyDerivPath dir l r) where
  pretty (InjectHoleyDerivPath dpath) = pretty dpath
  pretty (HoleInteriorHoleyDerivPath dpath) = Expr.prettyPath dpath $ "(⌶{?} : _)"

moveHoleyDerivZipper :: forall l r. IsRuleLabel l r => MoveDir -> HoleyDerivZipper l r -> Maybe (HoleyDerivZipper l r)
moveHoleyDerivZipper = case_
  # on _up (\_ -> Zippable.zipUp)
  # on _down (\_ -> Zippable.zipDown 0)
  # on _left (\_ -> Zippable.zipLeft)
  # on _right (\_ -> Zippable.zipRight)
  # on _prev (\_ -> Zippable.zipPrev)
  # on _next (\_ -> Zippable.zipNext 0)

hdzipperDerivPath :: forall l r. HoleyDerivZipper l r -> DerivPath Up l r
hdzipperDerivPath (InjectHoleyDerivZipper dzipper) = Expr.zipperPath dzipper
hdzipperDerivPath (HoleInteriorHoleyDerivZipper dpath _) = dpath

hdzipperHoleyDerivPath :: forall l r. HoleyDerivZipper l r -> HoleyDerivPath Up l r
hdzipperHoleyDerivPath (InjectHoleyDerivZipper dzipper) = InjectHoleyDerivPath (Expr.zipperPath dzipper)
hdzipperHoleyDerivPath (HoleInteriorHoleyDerivZipper dpath _) = HoleInteriorHoleyDerivPath dpath

hdzipperDerivTerm :: forall l r. IsRuleLabel l r => HoleyDerivZipper l r -> DerivTerm l r
hdzipperDerivTerm (InjectHoleyDerivZipper dzipper) = Expr.zipperExpr dzipper
hdzipperDerivTerm (HoleInteriorHoleyDerivZipper dpath sort) = assert (just "hdzipperDerivTerm" (defaultDerivTerm sort)) \dterm ->
  -- Expr.unzipper $ Expr.Zipper dpath dterm
  dterm

hdzipperZipper :: forall l r. IsRuleLabel l r => HoleyDerivZipper l r -> DerivZipper l r
hdzipperZipper hdzipper = do
  let path = hdzipperDerivPath hdzipper
  let dterm = hdzipperDerivTerm hdzipper
  Expr.Zipper path dterm

escapeHoleInterior :: forall l r. IsRuleLabel l r => Cursor l r -> Cursor l r
escapeHoleInterior cursor = do
  let path = hdzipperDerivPath cursor.hdzipper
  let dterm = hdzipperDerivTerm cursor.hdzipper
  cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper path dterm))

defaultEditsAtHoleyDerivZipper :: forall l r. IsRuleLabel l r => Sort l -> HoleyDerivZipper l r -> Array (Edit l r)
defaultEditsAtHoleyDerivZipper topSort = case _ of
  InjectHoleyDerivZipper dz -> defaultEditsAtDerivZipper topSort dz
  HoleInteriorHoleyDerivZipper p sort -> defaultEditsAtHoleInterior p sort

_verbose_path_element_ids :: Boolean
_verbose_path_element_ids = true

fromHoleyDerivPathToElementId :: forall l r. IsRuleLabel l r => HoleyDerivPath Up l r -> String
fromHoleyDerivPathToElementId
  | _verbose_path_element_ids = case _ of
      InjectHoleyDerivPath dpath -> fromPathToElementId dpath
      HoleInteriorHoleyDerivPath dpath -> dpath # Expr.foldMapPath "holeInterior-PathNil" \(Expr.Tooth l kidsZip) str -> String.replaceAll (String.Pattern " ") (String.Replacement "_") (pretty l <> "@" <> show (ZipList.leftLength kidsZip)) <> "-" <> str
  | otherwise = case _ of
      InjectHoleyDerivPath dpath -> fromPathToElementId dpath
      HoleInteriorHoleyDerivPath dpath -> dpath # Expr.foldMapPath "holeInterior-PathNil" \(Expr.Tooth l kidsZip) str -> show (ZipList.leftLength kidsZip) <> "-" <> str

fromPathToElementId :: forall l. Expr.IsExprLabel l => Expr.Path Up l -> String
fromPathToElementId 
  | _verbose_path_element_ids = Expr.foldMapPath "PathNil" \(Expr.Tooth l kidsZip) str -> String.replaceAll (String.Pattern " ") (String.Replacement "_") (pretty l <> "@" <> show (ZipList.leftLength kidsZip)) <> "-" <> str
  | otherwise = Expr.foldMapPath "PathNil" \(Expr.Tooth _ kidsZip) str -> show (ZipList.leftLength kidsZip) <> "-" <> str

cursorClassName = "cursor" :: String
highlightClassName = "highlight" :: String
selectTopClassName = "select-top" :: String
selectBottomClassName = "select-bottom" :: String

-- | Buffer

type BufferInput l r =
  { hdzipper :: HoleyDerivZipper l r
  , edits :: Array (EditAndPreview l r)
  }

bufferSlot = Proxy :: Proxy "buffer"

isBufferKey :: String -> Boolean
isBufferKey = (_ `Array.elem` [" ", "Enter"])

type EditAndPreview l r = 
  { edit :: Edit l r
  , lazy_preview :: Lazy (EditPreviewHTML l r)
  }

data Output l r
  = ActionOutput (Action l r)
  -- | UpdateStateOutput (State l r -> HK.HookM Aff (State l r))
  | SetPreviewOutput (Maybe (EditPreviewHTML l r))

-- data Preview l r
--   = EmptyPreview
--   | WrapPreview {before :: Array (EditorHTML l r), after :: Array (EditorHTML l r)}
--   | ReplacePreview (Array (EditorHTML l r))

data Query a
  -- = KeyboardEvent KeyboardEvent.KeyboardEvent a
  = SetBufferEnabledQuery Boolean (Maybe String) a
  -- | SetBufferStringQuery String a
  | MoveBufferQuery VerticalDir a
  | SubmitBufferQuery a


-- | Preview

previewSlot = Proxy :: Proxy "preview"

data PreviewQuery l r a
  = SetPreviewQuery (Maybe (EditPreviewHTML l r)) a
