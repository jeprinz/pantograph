module Language.Pantograph.Generic.Rendering.Base where

import Prelude
import Type.Direction

import Bug.Assertion (assert, just)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Either.Nested (type (\/))
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
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML (ComponentHTML) as HH
import Halogen.Hooks as HK
import Language.Pantograph.Generic.Edit (Action, Edit, defaultEditsAtDerivZipper, defaultEditsAtHoleInterior)
import Language.Pantograph.Generic.Grammar (class IsRuleLabel, DerivPath, DerivTerm, DerivZipper, DerivZipperp, Sort, defaultDerivTerm, isHoleDerivTerm)
import Text.Pretty (class Pretty, pretty)
import Text.Pretty as P
import Type.Proxy (Proxy(..))

type EditorHTML l r = HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot (Query) (Output l r) String) Aff

type DerivTermPrerenderer l r = 
  { rule :: r
  , sort :: Sort l
  , kids :: Array (DerivTerm l r)
    -- TODO: To deal with newlines, replace "Array HTML -> ..." with "Array (Maybe Int -> HTML) -> ...". Nothing = same line, Just n = newline with n (additional, relative) tabs
  -- , kidElems :: Array (EditorHTML l r) }
  }
  -> 
  { classNames :: Array String
  -- Each item of the array is either a reference to a rendered kid (via its
  -- index in `kids`) or an array of any html
  , subElems :: Array (Int \/ Array (EditorHTML l r)) }

type EditorSpec l r =
  { hdzipper :: HoleyDerivZipper l r
  , topSort :: Sort l
  , editsAtHoleyDerivZipper :: Sort l -> HoleyDerivZipper l r -> Array (Edit l r)
  
  -- -- the output terms are valid (already checked via unification when generated)
  -- !TODO editsAtHoleInterior :: Sort l -> Array (String /\ Sub l /\ DerivTerm l r)
  
  -- -- corresponds to a change where the path is inserted and the topChange is
  -- -- inserted as a boundary at the top, and likewise for botChange, and them
  -- -- smallstep figured out the final result
  -- !TODO editsAtCursor :: Sort l -> Array {label :: String, topChange :: Change l r, path :: Path, botChange :: Change}

  -- !TODO isValidCursorSort :: Grammar.Sort l -> Boolean
  -- !TODO isValidSelectionSorts :: Grammar.Sort l -> Grammar.Sort l -> Boolean
  
  , prerenderDerivTerm :: DerivTermPrerenderer l r
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

type BufferInput l r =
  { hdzipper :: HoleyDerivZipper l r
  , edits :: Array (Lazy (EditorHTML l r) /\ Edit l r)
  }

bufferSlot = Proxy :: Proxy "buffer"

isBufferKey :: String -> Boolean
isBufferKey = (_ `Array.elem` [" ", "Enter"])

data Output l r
  = ActionOutput (Action l r)
  | UpdateFacadeOutput (State l r -> HK.HookM Aff (State l r))

data Query a
  -- = KeyboardEvent KeyboardEvent.KeyboardEvent a
  = SetBufferEnabledQuery Boolean (Maybe String) a
  -- | SetBufferStringQuery String a
  | MoveBufferQuery VerticalDir a
  | SubmitBufferQuery a

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
