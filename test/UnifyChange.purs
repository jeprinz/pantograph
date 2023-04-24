module Test.UnifyChange where

import Data.Gram
import Data.Tuple.Nested
import Prelude

import Control.Monad.State (StateT, runState, runStateT)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List.Zip (unpathAround)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Unify (class Unify, unify)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)

main :: Effect Unit
main = do
  log "[Test.UnifyChange]"

  tryUnifyChange ex_ch1 ex_ch2
  
  pure unit

type L = Name

newtype UnifyM a = UnifyM (StateT {unifySub :: Map.Map MetaVar (UnifyM (MetaChange L))} Maybe a)

substUnifyM :: MetaChange L -> UnifyM (MetaChange L)
substUnifyM = unsafeThrow "TODO"


instance Unify UnifyM (UnifyM (MetaChange L)) where
  unify m_ch1 m_ch2 = do
    ch1 <- m_ch1
    ch2 <- m_ch2
    substUnifyM =<< unify m_ch1 m_ch2

tryUnifyChange :: MetaChange L -> MetaChange L -> Effect Unit
tryUnifyChange ch1 ch2 = case flip runStateT {unifySub: Map.empty :: Map.Map MetaVar (Meta L)} $ unify ch1 ch2 of
  Nothing -> log $ "failed to unify:\n - ch1 = " <> show ch1 <> "\n - ch2 = " <> show ch2
  Just (ch /\ _) -> log $ "successfully unified:\n - ch1 = " <> show ch1 <> "\n - ch2 = " <> show ch2 <> "\n - result = " <> show ch

ex_ch1 :: MetaChange L
ex_ch1 = 
  exprChange (fromConcrete (Name "Arrow"))
    [ fromConcrete (exprChange (Name "A") [])
    , fromMetaVar (freshMetaVar unit) 
    ]

ex_ch2 :: MetaChange L
ex_ch2 = 
  exprChange (fromConcrete (Name "Arrow")) 
    [ fromMetaVar (freshMetaVar unit) 
    , fromConcrete (exprChange (Name "B") [])
    ]

-- showChange :: forall l. Show l => Change l -> String
-- showChange ch = matchChange ch 
--   { expr: \l kids -> 
--       if Array.null kids
--         then show l
--         else "(" <> show l <> " " <> intercalate " " (showChange <$> kids) <> ")"
--   , minus: \(l /\ p) kid -> "(- " <> show l <> " " <> intercalate " " (unpathAround (showChange kid) (show <$> p)) <> ")"
--   , plus: \(l /\ p) kid -> "(+ " <> show l <> " " <> intercalate " " (unpathAround (showChange kid) (show <$> p)) <> ")"
--   , replace: \e1 e2 -> "(" <> show e1 <> " ~~> " <> show e2 <> ")"
--   }
