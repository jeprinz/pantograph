module Data.Gram2.Example1 where

import Data.Tuple.Nested
import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Gram2 (Gram(..), showMetaVar)
import Data.Gram2 as Gram
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Partial.Unsafe (unsafeCrashWith)

-- newtype Var = MakeVar String

-- instance Show Var where show (MakeVar str) = "(Var " <> str <> ")"

-- showVar :: Var -> String
-- showVar (MakeVar str) = str

-- data Label 
--   = -- var binds
--     TermBind Var
--     -- terms 
--   | VarTerm Var | LamTerm | AppTerm
--     -- types
--   | BaseType | ArrType

-- derive instance Generic Label _
-- instance Show Label where show x = genericShow x

-- type Expr = Gram.Expr Label

-- varTerm :: Var -> Expr 
-- varTerm v = Gram $ wrap $ VarTerm v /\ []

-- lamTerm :: Var -> Expr -> Expr
-- lamTerm v e = Gram $ wrap $ LamTerm /\ [Gram $ wrap $ TermBind v /\ [], e]

-- appTerm :: Expr -> Expr -> Expr 
-- appTerm e1 e2 = Gram $ wrap $ AppTerm /\ [e1, e2]

-- baseType :: Expr
-- baseType = Gram $ wrap $ BaseType /\ []

-- arrType :: Expr -> Expr -> Expr
-- arrType e1 e2 = Gram $ wrap $ ArrType /\ [e1, e2]

-- matchExpr 
--     { termBind
--     , varTerm, lamTerm, appTerm
--     , baseType, arrType }
--   = case _ of
--   -- binds
--   TermBind v /\ [] -> termBind v
--   -- terms
--   VarTerm v /\ [] -> varTerm v
--   LamTerm /\ [e1, e2] -> lamTerm e1 e2
--   AppTerm /\ [e1, e2] -> appTerm e1 e2
--   -- types
--   BaseType /\ [] -> baseType
--   ArrType /\ [e1, e2] -> arrType e1 e2
--   _ -> unsafeCrashWith "invalid expression"

-- -- showExpr' ∷ ∀ (t198 ∷ Type) (t199 ∷ Type -> Type) (t200 ∷ Type -> Type). 
-- --   Functor t199 ⇒ Functor t200 ⇒ 
-- --   (t199 (Tuple t198 (t200 String)) → Tuple Label (Array String)) → 
-- --   Gram t198 t199 t200 → String
-- -- showExpr' unjoint = Gram.foldMapGram $ unjoint >>> matchExpr
-- --   { termBind: \v -> showVar v
-- --   , varTerm: \v -> showVar v
-- --   , lamTerm: \v e -> "(λ " <> v <> " => " <> e <> ")"
-- --   , appTerm: \e1 e2 -> "(" <> e1 <> " " <> e2 <> ")"
-- --   , baseType: "*"
-- --   , arrType: \e1 e2 -> "(" <> e1 <> " -> " <> e2 <> ")"
-- --   }

-- ex_term1 :: Expr 
-- ex_term1 = lamTerm x (varTerm x)
--   where x = MakeVar "x"

-- type MetaExpr = Gram.MetaExpr Label 

-- -- showMetaExpr :: MetaExpr -> String
-- -- showMetaExpr = Gram.foldMapGram $ case _ of 
-- --   Left mv -> showMetaVar mv
-- --   -- e :: Tuple Label (Array String)
-- --   Right e -> showExpr' ?a (Gram ?e) -- showExpr' ?a (Gram e) -- showExpr' ?a ?e


-- type Path step = Gram.Path Label step
-- type PathUp = Gram.PathUp Label
-- type PathDown = Gram.PathDown Label

-- type Change = Gram.Change Label

