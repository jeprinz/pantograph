module Language.Pantograph.Grammar where

import Prelude

{-
1) define the grammar
2) define what terms are using the grammar
3) define what teeth are using the grammar
-}

{-
The following is a list of the grammatical sorts within this editor:
Term, Type, TypeArg, TypeBind, TermBind, (List TypeBind)
-}

data UUID

data Change
--data Type x =

--subType :: (Map HoleId HoleID) -> Type -> Type

data Label x
    -- Terms
    = Var x
    | App {-Term-} {-Term-}
    -- Types
    | TVar x
    -- TypeArg
    -- TypeBind
    -- TermBind
    -- List TypeBind
    -- other

data Tuple a b = Tuple a b
derive instance Functor (Tuple a)

derive instance Functor Label

--data ExprWrap = ExprWrap Boolean Expr Expr
data Expr x = Expr (Label x) (Array (Wrapper (Expr x)))
--data Tooth f = Tooth Label (Array (f (Expr f))) (Array (f (Expr f)))

derive instance Functor Expr

--data Wrapper e = SameLine e | NewLine e
type Wrapper = Tuple Boolean

subExample :: (UUID -> Expr UUID) -> Expr UUID -> Expr _
subExample = map
--subExample :: (UUID -> Expr) -> Expr -> Expr
--subExample sub (Expr (Var id) []) = sub id
--subExample sub (Expr l children) = Expr l (map (\(Tuple md child) -> Tuple md $ subExample sub child) children)
--
--genMapOverExpr :: (UUID -> Expr) -> Expr -> Expr
--genMapOverExpr sub (Expr (Var id) []) = sub id
--genMapOverExpr sub (Expr l children) = Expr l (map (\(Tuple md child) -> Tuple md $ subExample sub child) children)



--app :: Expr -> Expr -> Expr
--app t1 t2 = Expr App [t1, t2]


--class Not a b | a -> b
--
--instance Not "asdfjkaslkdfj" Void
--instance Not Void Unit
--
