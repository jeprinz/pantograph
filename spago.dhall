{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "pantograph"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "const"
  , "control"
  , "css"
  , "debug"
  , "dom-indexed"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "functors"
  , "fuzzy"
  , "halogen"
  , "halogen-css"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "identity"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "rationals"
  , "record"
  , "refs"
  , "safe-coerce"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  , "unicode"
  , "unsafe-coerce"
  , "uuid"
  , "variant"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
