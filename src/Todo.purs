module Todo (class TodoWarning, todo) where

import Prim.TypeError (class Warn, Text)

class TodoWarning

instance warnTodoWarning :: Warn (Text "Contains TODOs") => TodoWarning

foreign import _todo :: forall a b. a -> b

todo :: forall a b. TodoWarning => a -> b
todo a = _todo a
