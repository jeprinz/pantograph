module Language.Pantograph.Generic.Rendering.Base where

import Language.Pantograph.Generic.Edit
import Language.Pantograph.Generic.Grammar
import Prelude
import Type.Direction

import Bug.Assertion (Assertion(..), assert, just)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Expr (class ReflectPathDir)
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy)
import Data.List (List)
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
import Language.Pantograph.Generic.Smallstep (StepRule, SSTerm)
import Language.Pantograph.Generic.Unification (Sub)
import Text.Pretty (class Pretty, pretty)
import Text.Pretty as P
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent as MouseEvent
import Debug (trace)
import Util as Util

type EditorHTML l r = 
  HH.ComponentHTML 
    (HK.HookM Aff Unit)
    ( buffer :: H.Slot Query (Output l r) Unit
    , preview :: H.Slot (PreviewQuery l r) Unit HorizontalDir
    , console :: H.Slot (Const Void) Void Unit
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
  , metavarNumbers :: Util.Stateful (Map.Map Expr.MetaVar Int /\ Int)
  }

getMetavarNumber :: RenderingContext -> Expr.MetaVar -> Int
getMetavarNumber renCtx mv =
    let map /\ max = (renCtx.metavarNumbers.get unit) in
    case Map.lookup mv map of
        Just n -> n
        Nothing ->
            let _ = renCtx.metavarNumbers.set (Map.insert mv max map /\ (max + 1)) in
            max

incremementIndentationLevel :: RenderingContext -> RenderingContext
incremementIndentationLevel ctx = ctx {indentationLevel = ctx.indentationLevel + 1}

data Linebreak
  = IndentedLinebreak
  | UnindentedLinebreak

defaultRenderingContext :: Unit -> RenderingContext
defaultRenderingContext unit =
  { indentationLevel: 0
  , isCursor: false
  , isInteractive: true
  , isInlined: false
  , metavarNumbers: Util.stateful (Map.empty /\ 0)
  }

previewRenderingContext :: Unit -> RenderingContext
previewRenderingContext unit =
  { indentationLevel: 0
  , isCursor: false
  , isInteractive: false
  , isInlined: true
  , metavarNumbers: Util.stateful (Map.empty /\ 0)
  }

type ArrangeDerivTermSubs l r =
  Partial =>
  { mb_parent :: Maybe (DerivTooth l r)
  , renCtx :: RenderingContext
  , rule :: r
  , sigma :: (Expr.MetaVarSub (Sort l)) -- The substitution on the DerivLabel
  , sort :: Sort l
--  , renderTerm :: DerivTerm l r -> (whatever type stuff like colonElem has) -- maybe this could be used to render the type in TermHole?
  } -> 
  Array (PreKid l r)

type PreKid l r = 
  (RenderingContext /\ Int) \/ -- reference to kid, with kid's rendering context
  Array (EditorHTML l r) -- static html

type SplitChangeType l = SortChange l -> {downChange :: SortChange l, upChange :: SortChange l, cursorSort :: Sort l}

type EditorSpec l r =
  { dterm :: DerivTerm l r

  -- inputChange = downChange o upChange
  -- cursorSort is the right endpoint of downChange and left endpoint of upChange
  -- cursorSort represents the sort at which such a path could be inserted, or alternately
  -- the new cursor sort after deletion of the path
   , splitChange :: SplitChangeType l

--  -- | removePathChanges: change → (downChange, upChange, cursorSort)
--  -- | The input change is the change going up the path to be deleted
--  -- | cursorSort is the left endpoint of downChange and upChange
--  -- | subject to the constraint that   downChange^-1 o upchange = change
--  , removePathChanges ::
--      SortChange l ->
--      {downChange :: SortChange l, upChange :: SortChange l, cursorSort :: Sort l}

  -- The output terms are valid (already checked via unification when
  -- generated).
  , editsAtHoleInterior :: Sort l -> Array (Edit l r)
  
  -- Corresponds to a change where the path is inserted and the topChange is
  -- inserted as a boundary at the top, and likewise for botChange, and them
  -- smallstep figured out the final result.
  , editsAtCursor :: Sort l -> Array (Edit l r)

  , isValidCursorSort :: Sort l -> Boolean

  -- TODO: I don't think that this is general enough for all languages, but its fine for now
  , isValidSelectionSorts :: {bottom :: Sort l, top :: Sort l} -> Boolean
  
  , arrangeDerivTermSubs :: Unit -> ArrangeDerivTermSubs l r

  , stepRules :: List (StepRule l r)

  , languageChanges :: LanguageChanges l r

  -- Input Sort is that of the term to be deleted, output Change is propagated upwards after deletion.
  , onDelete :: Sort l -> SortChange l
    -- default is ChangeAlgebra.inject

  , generalizeDerivation :: Sort l -> SortChange l
    -- default is ChangeAlgebra.inject

  , specializeDerivation :: {-clipboard-} Sort l -> {-cursor-} Sort l -> SortChange l
    -- default is ChangeAlgebra.inject

  , forgetSorts :: DerivLabel l r -> Maybe (DerivLabel l r)
    -- default is (const Nothing)

  , clipboardSort :: Sort l -> Sort l
    -- default is identity

  -- TODO: find a way to put defaultDerivTerm in EditorSpec instead of a TypeClass. Yes I know it requires re-plumbing some things.

  }

editsAtHoleyDerivZipper :: forall l r. IsRuleLabel l r => EditorSpec l r -> HoleyDerivZipper l r -> Array (Edit l r)
editsAtHoleyDerivZipper spec hdzipper = case hdzipper of
  InjectHoleyDerivZipper dz -> spec.editsAtCursor (derivZipperSort dz)
  HoleInteriorHoleyDerivZipper _ label -> spec.editsAtHoleInterior (derivLabelSort label)

-- -- Stuff that's defined inside of the editor component
-- type EditorLocals l r = 
--   { clipboard_ref :: Ref.Ref (Maybe (Either (DerivPath Up l r) (DerivTerm l r)))
--   , currentState :: State l r
--   , facade_ref :: Ref.Ref (State l r)
--   , initState :: State l r
--   , input :: EditorSpec l r
--   , maybeHighlightPath_ref :: Ref.Ref (Maybe (HoleyDerivPath Up l r))
--   , state_id :: HK.StateId (State l r)
--   }

data State l r
  = CursorState (Cursor l r)
  | SelectState (Select l r)
  | TopState (Top l r)
  | SmallStepState (SmallStepState l r) 

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
    SmallStepState ssstate -> Array.intercalate "\n"
      [ "small stepping:"
      , P.indent $ P.newlines
          [ "- ssterm = " <> pretty ssstate.ssterm
          ]
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

type SmallStepState l r =
  { ssterm :: SSTerm l r
  }

data HoleyDerivZipper l r
  = InjectHoleyDerivZipper (DerivZipper l r)
  | HoleInteriorHoleyDerivZipper 
      (DerivPath Up l r) -- the path to the Hole
--      (Sort l) -- the sort of the Hole
      (DerivLabel l r) -- the rule of the Hole

isValidCursor :: forall l r. IsRuleLabel l r => EditorSpec l r -> HoleyDerivZipper l r -> Boolean
isValidCursor spec (HoleInteriorHoleyDerivZipper _ _) = true
isValidCursor spec (InjectHoleyDerivZipper dz) = spec.isValidCursorSort (derivZipperSort dz)

isValidSelect :: forall l r. IsRuleLabel l r => EditorSpec l r -> DerivZipperp l r -> Boolean
isValidSelect spec (Expr.Zipperp dpath selection dterm) =
    let bottom = derivTermSort dterm in
    let upSelection = case selection of
            Left p -> Expr.reversePath p
            Right p -> p
    in
--    trace ("in isValidSelect, bottom is " <> pretty bottom <> " and top is " <> pretty (derivPathSort upSelection bottom)) \_ ->
    spec.isValidSelectionSorts {bottom, top: derivPathSort upSelection bottom}

derive instance Generic (HoleyDerivZipper l r) _
derive instance (Eq l, Eq r) => Eq (HoleyDerivZipper l r)
derive instance (Ord l, Ord r) => Ord (HoleyDerivZipper l r)
instance (Show l, Show r) => Show (HoleyDerivZipper l r) where show x = genericShow x

instance IsRuleLabel l r => Pretty (HoleyDerivZipper l r) where
  pretty (InjectHoleyDerivZipper dzipper) = pretty dzipper
  pretty (HoleInteriorHoleyDerivZipper dpath label) = Expr.prettyPath dpath $ "(⌶{?} : " <> pretty label <> ")"

instance IsRuleLabel l r => Zippable (HoleyDerivZipper l r) where
  zipDowns (InjectHoleyDerivZipper dz) | Just sort <- isHoleDerivTerm (Expr.zipperExpr dz) = do
    [HoleInteriorHoleyDerivZipper (Expr.zipperPath dz) (Expr.exprLabel (Expr.zipperExpr dz))]
  zipDowns (InjectHoleyDerivZipper dz) = InjectHoleyDerivZipper <$> Zippable.zipDowns dz
  zipDowns (HoleInteriorHoleyDerivZipper _ _) = []
  zipUp' (InjectHoleyDerivZipper dz) = bimap identity InjectHoleyDerivZipper <$> Zippable.zipUp' dz
--  zipUp' (HoleInteriorHoleyDerivZipper dpath sort) =
--    assert (just "Zippable (HoleyDerivZipper l r) . zipUp'" (defaultDerivTerm sort)) \dterm ->
--      pure $ 0 /\ InjectHoleyDerivZipper (Expr.Zipper dpath dterm)
  zipUp' (HoleInteriorHoleyDerivZipper dpath label) =
    pure $ 0 /\ InjectHoleyDerivZipper (Expr.Zipper dpath (Expr.Expr label []))

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

-- Given a function that says if cursor positions are valid, move until something is valid (or move nowhere if nothing is valid)
moveHDZUntil :: forall l r. IsRuleLabel l r => MoveDir
    -> (HoleyDerivZipper l r -> Boolean)
    -> HoleyDerivZipper l r
    -> Maybe (HoleyDerivZipper l r)
moveHDZUntil dir valid hdz =
    case moveHoleyDerivZipper dir hdz of
        Just hdz' ->
            if valid hdz' then Just hdz' else moveHDZUntil dir valid hdz'
        Nothing -> Nothing

hdzipperDerivPath :: forall l r. HoleyDerivZipper l r -> DerivPath Up l r
hdzipperDerivPath (InjectHoleyDerivZipper dzipper) = Expr.zipperPath dzipper
hdzipperDerivPath (HoleInteriorHoleyDerivZipper dpath _) = dpath

hdzipperHoleyDerivPath :: forall l r. HoleyDerivZipper l r -> HoleyDerivPath Up l r
hdzipperHoleyDerivPath (InjectHoleyDerivZipper dzipper) = InjectHoleyDerivPath (Expr.zipperPath dzipper)
hdzipperHoleyDerivPath (HoleInteriorHoleyDerivZipper dpath _) = HoleInteriorHoleyDerivPath dpath

hdzipperDerivTerm :: forall l r. IsRuleLabel l r => HoleyDerivZipper l r -> DerivTerm l r
hdzipperDerivTerm (InjectHoleyDerivZipper dzipper) = Expr.zipperExpr dzipper
hdzipperDerivTerm (HoleInteriorHoleyDerivZipper dpath label) = Expr.Expr label []
--  assert (just "hdzipperDerivTerm" (defaultDerivTerm sort)) \dterm ->
--  -- Expr.unzipper $ Expr.Zipper dpath dterm
--  dterm

hdzipperDerivZipper :: forall l r. IsRuleLabel l r => HoleyDerivZipper l r -> DerivZipper l r
hdzipperDerivZipper hdzipper = do
  let path = hdzipperDerivPath hdzipper
  let dterm = hdzipperDerivTerm hdzipper
  Expr.Zipper path dterm

escapeHoleInterior :: forall l r. IsRuleLabel l r => Cursor l r -> Cursor l r
escapeHoleInterior cursor = do
  let path = hdzipperDerivPath cursor.hdzipper
  let dterm = hdzipperDerivTerm cursor.hdzipper
  cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper path dterm))

{-
defaultEditsAtHoleyDerivZipper :: forall l r. IsRuleLabel l r => Sort l -> HoleyDerivZipper l r -> Array (Edit l r)
defaultEditsAtHoleyDerivZipper topSort = case _ of
  InjectHoleyDerivZipper dz -> defaultEditsAtCursor (derivPathSort topSort (Expr.zipperPath dz))
  HoleInteriorHoleyDerivZipper _ sort -> defaultEditsAtHoleInterior sort
-}

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

------------------------------------------------------------------------------
-- Editor local functions
------------------------------------------------------------------------------

type EditorLocals l r = 
  { spec :: EditorSpec l r
  , handleBufferOutput :: Output l r -> HK.HookM Aff Unit
  , onMouseDown :: HoleyDerivZipper l r -> MouseEvent.MouseEvent -> HK.HookM Aff Unit
  , onMouseOver :: HoleyDerivZipper l r -> MouseEvent.MouseEvent -> HK.HookM Aff Unit
  }

trivialEditorLocals :: forall l r. EditorSpec l r -> EditorLocals l r
trivialEditorLocals spec =
  { spec
  , handleBufferOutput: \_ -> pure unit
  , onMouseDown: \_ _ -> pure unit
  , onMouseOver: \_ _ -> pure unit
  }

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
