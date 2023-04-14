module Language.Pantograph.Grammar where

import Prelude
import Prim hiding (Type)
import Language.Pantograph.Expression
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.List as List
import Data.Maybe (Maybe)
import Data.Tuple.Nested

{-
1) define the grammar
2) define what terms are using the grammar
3) define what teeth are using the grammar
-}

{-
The following is a list of the grammatical sorts within this editor:
Term, Type, TypeBind, TermBind, (List TypeBind)
InnerTermHole, InnerTypeHole
-}

type UUID = Int

--data Type x =

--subType :: (Map HoleId HoleID) -> Type -> Type

--data Sort
--    = STerm Type -- The Value here is a Type
--    | SType
--    | STermBind
--    | STypeBind
--    | SListTypeBind
--    | SInnerTermHole
--    | SInnerTypeHole

newtype TermVarID = TermVarID UUID
newtype TypeVarID = TypeVarID UUID
newtype HoleID = HoleID UUID

--freshTermID :: Unit -> TermVarID
--freshTermID = undefined
--freshTypeID :: Unit -> TypeVarID
--freshTypeID = undefined
--freshHoleID :: Unit -> HoleID
--freshHoleID = undefined

data TypeVarName = TypeVarName String

data TypeVar = TypeVar TypeVarID | CtxBoundaryTypeVar Kind TypeVarName TypeVarID -- TypeVar represents a variable in scope, and CtxBoundaryTypeVar represents a variable inside a context boundary Insert, with the given type.

data PolyType
data Kind
type Type = Value

data Label
    -- Terms
    = Var TermVarID
    | App {-Term-} {-Term-}
    | Lambda TermVarID {-Term-}
    | Let TermVarID (List TypeVarID) {-TermBind-} {-Term-} {-Type-} {-Term-}
    | TypeBoundary Change {-Term-}
    | ContextBoundary {-Some kind of change TBD-} {-Term-}
    | Hole
    -- No Buffers? Seems simpler to use TypeBoundaries everywhere?
    -- Types
    | TNeu TypeVar {-List TypeArg-}
    | Arrow {-Type-} {-Type-}
    | THole HoleID (Map TypeVarID Type)
    -- TypeBind
    | TypeBind TermVarID
    -- TermBind
    | TermBind TermVarID
    -- List TypeBind
    | ListTypeBindNil
    | ListTypeBindCons {-TypeBind-} {-List TypeBind-}

    -- I don't know how to make this a separate grammar...?
    -- Sorts
    | STerm {-Type-}
    | SType
    | STermBind
    | STypeBind
    | SListTypeBind
    | SInnerTermHole
    | SInnerTypeHole

data Tuple a b = Tuple a b

--data ExprWrap = ExprWrap Boolean Expr Expr
--data Tooth f = Tooth Label (Array (f (Expr f))) (Array (f (Expr f)))

newtype WrapMetadata = WrapMetadata {indented:: Boolean, sort:: Value}
type Value = Expr Label WrapMetadata
type ProgramPath = Path Label WrapMetadata

type Change = GChange Label


type TypingRuleEntry = GTypingRuleEntry Change UUID
type TypingRule = GTypingRule Change UUID


--        [G |- (Replace (Term (A -> B)) Type),     G, + X : A  |-  Term (+A -> B)]
typingRules :: Array TypingRule
typingRules = [
    -- Lambda
    let x = 0 in -- obviously, in reality these should use fresh UUIDs and not just 0, 1, and 2
    let a = 1 in
    let b = 2 in
    TypingRule [
        TypingRuleEntry (Map.empty) (Replace (ExprWM STerm [EMetaVar a , EMetaVar b]) (ExprWM SType [])),
        TypingRuleEntry (Map.insert x (MetaVar a) Map.empty) (ChangeExpr STerm [Plus Arrow [MetaVar a] (MetaVar b) []])
        ]
]

{-

--labelBindVarInfo :: Label -> LabelInfo
--labelBindVarInfo (Lambda x) = Binds [[x]]
--labelBindVarInfo (Var id) = IsVar id
--labelBindVarInfo _ = Nothing

-- It should be possible to define typechecking fairly generically like this.
-- This single function should be able to check both terms and paths!
typeCheck :: Label -> Sort -> Array Sort -> Boolean
typeCheck App (STerm outTy) [(STerm arrTy), (STerm argTy)] = ?h -- Check that arrTy == argTy -> outTy
typeCheck (Let tBind tyBinds) (STerm ty) [STermBind, (STerm defTy), SType, (STerm bodyTy)]
    = true -- check if ty == bodyTy, and more stuff I guess
-- ...
typeCheck _ _ _ = false


-}