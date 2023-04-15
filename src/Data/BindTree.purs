module Data.BindTree where

import Prelude
import Control.Monad.Reader (runReader)
import Control.Monad.State (evalState, gets, modify, modify_, runState)
import Data.Bitraversable (class Bitraversable, bitraverse)
import Data.Map as Map
import Data.Set as Set
import Partial.Unsafe (unsafeCrashWith)

data BindInfo bind use
  = BindInfo
    { binds :: Set.Set bind
    , uses :: Set.Set use
    }

emptyBindInfo :: forall bind use. BindInfo bind use
emptyBindInfo = BindInfo { binds: Set.empty, uses: Set.empty }

newtype BindTree th (bind :: Type) (use :: Type)
  = BindTree (th bind use (BindTree th bind use))

-- mapBindTree 

-- class BindTree t where
--   var :: forall bind use. use -> t bind use
--   bindInfo :: forall bind use. t bind use -> BindInfo bind use
-- mapBindTree' ::
--   forall t bind bind' use use'.
--   BindTree t =>
--   Bitraversable t =>
--   Ord bind =>
--   (bind -> bind') ->
--   (bind -> use -> use') ->
--   ((use -> use') -> use -> use') ->
--   t bind use ->
--   t bind' use'
-- mapBindTree' f1 f2 f3 t =
--   runReader
--     ( bitraverse
--         ( \bind -> do
--             let
--               bind' = f1 bind
--             -- modify_ $ Map.insert bind bind'
--             pure bind'
--         )
--         ( \use -> do
--             -- gets (Map.lookup )
--             ?a
--         )
--         t
--     )
--     ?xxx
