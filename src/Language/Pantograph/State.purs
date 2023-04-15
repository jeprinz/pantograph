module Language.Pantograph.State where

import Prelude
import Language.Pantograph.Grammar
import Data.Map (Map)

-- There is only one kind of CursorMode - what grammatical sort you're at is determined by "sort"
type CursorMode = {
    value :: Value
    , path :: ValuePath
    , sort :: Value
    , ctx :: Map UUID Value {- this Value is a Sort-}
}

type SelectMode = {
    value :: Value
    , sort1 :: Value
    , ctx1 :: Map UUID Value {- this Value is a Sort-}
    , path1 :: ValuePath
    , sort2 :: Value
    , ctx2 :: Map UUID Value {- this Value is a Sort-}
    , path2 :: ValuePath
}

data EditorState = CursorMode CursorMode | SelectMode SelectMode