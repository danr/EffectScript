* Effect types
    - Shallow handlers as in Frank (not automatically recursive as in Koka)
    - A handler returns a hidden and automatically generated data-type
      where the continuation is an argument, or return/final/pure/done
    - Partial effect for partial pattern-matching (or partial functions)
    - What is the type system for effects?
      What happens when the same label is used twice?
      Can there be two polymorphic tails?
      Can the offset for the handlers be known statically?
      What about linear handlers? Affine handlers?
      Those are the most common.

* Compilation using jump-lets as in Compiling without Continuations

* Strict by default. Lazy with memoizing force function.

* Syntax
    - Python-inspired
    - Indentation-sensitive
    - Functions are uncurried, type constructors curried (HKT supported)
    - Special syntax for calling functions with a function argument
    - Case (with pattern guards? disjunctive patterns?)
    - But otherwise try to keep the syntax very small to ease learning
      and solve problems with functions, data and effects.
        - Exception?: pattern synonyms with builtin view patterns
    - No restrictions on casing
    - Overloading on constructors and record projections are OK
    - Should probably support infix operators and possibly prefix.
    - Some sugar for types
    - Colon only used when introducing functions (in expression context),
      so case will terminate with of and branches with =>

* Docstrings and doctest support (typechecking and running) by the main compiler

* Uncurried functions
    - Nice, but how to write code generic over arity?
    - Should there be builtin tuple constructors too?
    - Maybe there could be typeclass instances for rest parameters
      as heterogenous lists (double-linked?). Prototype:

        ```haskell
        class Curry a b where
            uncurry : a -> b
            curry : b -> a

        instance Curry ((a1, a2) -> r) (a1 -> a2 -> r) where
            def uncurry(f): def(x1): def(x2): f(x1, x2)
            def curry(f): def(x1, x2): f(x1)(x2)

        instance Curry (*as -> r) k => Curry ((a, *as) -> r) (a -> k) where
            def uncurry(f: (a, *as) -> r) -> a -> k:
                def(x: a):
                    uncurry(def(*xs: *as): f(x, *xs))

            def curry(f: a -> k) -> (a, *as) -> r:
                def(x: a, xs: *as):
                    curry(f(x))(*xs)
        ```

    - One can then define for instance:

      ```python
      def apply(f: (a, *as) -> <e> r) -> a -> *as -> <e> r:
          def(a): def(*as): f(a, *as)
      ```

      The kind of a is Type, but the kind of *as is [Type] or something

    - Row types?


* Encapsulation via private/public modifiers
    - Possible to add package modifier too, or modifier for only child modules (protected)
    - No explicit export list as in Haskell
    - Reexport modules by and public imports (like open import in Agda)

* Typeclasses
    - Begin with single parameter typeclasses without type families or associated types,
      type equalities (and thus no GADTs yet neither)
    - Except for Ord, it seems like default implementations are not so useful,
      and that typeclasses could only contain one function
      For Ord, it's nice to call <, >, <=, => etc correctly and also be able to
      choose between implementing < and compare.
    - Not sure about the syntax for types with type class constraint contexts

* Rank 2 types
    - Only supported if it makes sense for effect types

* Existential types
    - Should ideally be supported at least in data declarations, but as part of
      the type syntax would be nice too

* Automatic newtype & other efficient complilation of small/enum datatypes

* Generics & RTTI
    - Could support both Haskell-style if the type system becomes strong enough,
      and PureScript otherwise.

* Macros/TH
    - Faster than generics as it gets compiled. Good for things like Geniplate.
    - Not so hard to implement as the language is small

* Self-hosting by Haskell interpreter which retains all type information for
  supporting type classes, avoiding type checking/inference and dynamic
  type errors.

* Compilation to LLVM and JS, and FFI to JS and C. Also asm.js and wasm via LLVM.
    - Primitive types needs to make some sense both in a JS and C/LLVM context.

* Interpretation could be done by V8 and LLI

* The compiler should be designed so that it can be called as an API for tooling

* Source locations should be preserved to allow source maps for JavaScript output

* The typechecker and renamer should be accessible so that language tools
  can be built (LSP & refactor)

* Errors should be postponed to as late as possible to provide feedback about
  as many errors as possible at the same time and also continue even in
  the presence of syntax, scope and type errors

* Can experiment with both mark-and-sweep and reference counting GC.
  Circular data can only be produced with explicit references with heap effects.

* Sensible standard library in scope by default. Strive for few imports.

* At some point consider extensible data types (and possibly records.)

* A standard formatter like gofmt

* Whole-program compilation possible, but also pre-compiled javascript/llbc
  for each module that can be used during development.
