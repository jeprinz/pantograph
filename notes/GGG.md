# GGG

Quesion: what to generic?

## Ungeneric

What was so annoying about working over specialized data?
- (basically, code repetition)
- typing traversal written for: expressions, paths, and changes
- changing functions became very complicated since there's a bulk of things to
  do in many places, using substitutions and recursively changing using the
  correctly matched arguments
- formatting traveral written for: expressions, path
- substitution manually defined over everything
- converting between tree-like representation and specialized representation
  - cursor movement
  - some other places
- generalizing change (associated with path in clipboard)

## Extremely Generic

At the extreme, we can generalize _everything_ to be runtime values -- that is, we
have a couple sum types
```purs
-- all the sorts of things
data Sort =
  -- term type
  = TermSort {ty :: Thing}
  | TypeSort | PolyTypeSort
  | TypeBindSort | TypeVarSort
  | TermBindSort | TermVarSort
  -- ... other basic sorts ...
  -- path
  | ToothSort {i :: Int} -- which child the tooth is "around"
  -- change
  | TypeChangeSort {tyIn :: Thing, tyOut :: Thing}
  | CtxChangeSort {ctxIn :: Thing, ctxOut :: Thing}
  -- ... other change sorts ...
  -- meta
  | MetaTermSort {ty :: Thing} -- used in changing rules
  | MetaTypeSort -- used in typing rules
  -- ... other meta sorts ...

-- all the things
data Thing label = Thing label (Array Thing) Sort
```
where `label` is the type of labels for the language (variants of types,
variants of terms, variants of typechanges, etc).

`Thing` is just a tree over `label /\ Sort`, so we might prefer to just use a
library-defined tree datatype instead of reinvent the tree.

The advantage of this form is that it _definitely_ handles everything -- its so
general that any possible operation over grammatical structures is expressible
as a function over `Thing`. 

However, `Thing` combines a bunch of data that is often operated on separately.
So, `Sort` encodes whatever useful grouping information is needed over `Thing`s,
as well as typing information such as the types of terms and the endpoint types
of typechanges. This info can be used for runtime assertions to dynamically
debug.

## Less Extremely Generic

Is there a way to slightly restruct the extremely generic approach to be
slightly less generic, to recover at least _some_ type-safety? There's a lot of
space here to experiment with, as Jacob and Henry have.

## Jacob

One way to generalize, as Jacob has in

- `/src/Language/Pantograph/Expression.purs`
- `/src/Language/Pantograph/GenericExpr/Expression.purs` (updated version?)

is to define a datatype for each of:
- Expr
- Tooth
- ExprWithMetaVars
- Change

There's some similar code that _would_ have to be written over each of (or some of)
these structures, in particular:
- typing traversal
- formatting traveral
- substitution manually defined over everything
- cursor movement
- ... some other things ...

However, Jacob simplifies this for the typing traversal by defining a datatype
encoding of typing rules, and then requiring only a simple function taht uses an
arbitrary set of rules to do a typing traversal over an Expr, Tooth, Change,
or ExprWithMetaVars.

You can do a similar thing for the formatting traversal, defining some
"formatting rules" in a way that can be interpreted to apply to both paths and
expressions.

Cursor movement is possibly more annoying, and begs again for a more general
tree-like structure (such as Jacob's TreeView from Pantograph 1). In Pantograph
2, cursor movement will be computed over some encoding of an index into the tree
rather than over expressions and paths directly. Some details to work out there.

What other things would we like to do generically, and does this encoding make
those easy?

## Henry

Another way to generalize, as Henry has, is to define one datatype `Gram`, which
each of the following are special cases of:
- Expr
- MetaExpr (expression with metavars)
- Path
- Change

There is a function `traverseGram` over `Gram` which gives a generic interface
for traversing over all of these datatypes. So, if you want to traverse over one
of them specifically, you can use the specialized function defined in terms of
`traverseGram`. If you want to work over `Gram` generically, then you can use
`traverseGram` itself. Essentially, `Expr`, `MetaExpr`, `Path`, and `Change` are
each subtypes of `Gram` via polymorphism.

The idea of encoding the typing rules as data is interesting. And if you can
actually derive the changing rules from just the the data encoding of the typing
rules then that'd be awesome. Haven't fully thought that through. 

The most direct alternative to that in this approach is to define a traversal in
terms of `traverseGram` that handles the typing logic, and then a traversal
that, given the relevant inputs, uses the typing logic traversal to implement
the changing algorithm.

If the changing algorithm _is_ just derivable from the typing rules, then its
less straightforward to take advantage of that in this approach than with the
data-encoded approach. The data-encoded approach could be used with `Gram` as
well.

What's nice about the `Gram` approach is that all of these
- typing traversal
- formatting traveral
- substitution manually defined over everything
- cursor movement
- ... some other things ...

dont require any special interfaces to work over `Gram` and it's subtypes -- you
just get the right interface by instantiating `Gram` enough (or not at all)
until it's as general as you want for your use case. It doesnt require any data
transformation to accomplish this, and so the abstraction has no overhead.


