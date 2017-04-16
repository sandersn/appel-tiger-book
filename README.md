Implementation of Appel's *modern compiler implemementation in ML*. In
a surprising twist, it is implemented in Purescript. This exists
mainly to help me learn Purescript.

* Chapter 1 shows off three things
    - How to use the default-generated pulp build.
    - How many monads you need to implement a tiny interpreter
    translated from ML or C.
    - How to squash all those monads into Purescript's Eff monad.
* I skip Chapters 2 and 3 because I've done enough parsing in my life.
* Instead, Chapters 4 and the Typescript module show
    - How to use Purescript's FFI.
    - How to convert a Javascript AST to a Purescript AST.
    - Never write a parser!
