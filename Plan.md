# EffectScript plan

* Focus areas:
    - Editor integration via LSP
        - `.` completes function, fields and submodules
        - `{` `[` record key completions
        - `:` record and case rhs
        - `(` `,` Signature help (also case?)
        - Target types can be triggered on `{` and `:`
        - Hover shows inferred types
        - Hover of top level constants shows evaluated values,
          on functions in their call chain the intermediate values.
        - Goto definition, find references
        - Highlighting constructors and effects specially (not in LSP sadly)
        - Automatic formatting
        - Lint-fixes... eta reduction? suggest to use { ... } call
        - Inferred type for range
        - Type holes, could trigger on `_` or hover on `_`
        - Type error messages with two source locations a'la Lennart
    - Syntax familiar to JS, TS, Python, C-family and Java programmers
    - Effect types
    - Integration with JS by translating TS definition files
        - Effect: huge chunk of browsers, nodejs and npm usable right away
    - JSON-serialization
        - Customizable
        - Parser reports errors
    - Row record types
    - Rank 2 types for lens and runST
    - Type-classes over row types
    - Seamless testing
        - package modifier for external testing specs
        - top level asserts that are run at compile time
        - built in doctest support
    - Macro system without writing AST
        - Derive TCs
        - Write boring code
        - Transform arbitrary string-dsls to code at compile-time
    - STM?
    - Parallelism?
* Postpone efficient compilation (except effect hierarchy)
    - Try to not introduce more overhead than JS
    - Goal is to be more efficient than python
    - Make decisions with regard for compilation efficiency,
      such as record fields being wholly compile-time
    - Need some compromise about JS numbers

## Syntax

### Types

Quantification only possible on function arrow.
Rationale: cannot have polymorphic constant.

```<A>(A, A) -> Bool```

Effects only possible on function arrow:
Rationale: canno have effectful constants

```<Eq A>(A, A) ->!console Bool```

### Label effects

Can label effects to be able to refer to them:

```() ->!<count: state<int>, depth: state<int>> Tree```

Now we can use `count.put(1)` and `depth.modify(x => x + 1)` etc

### Function syntax

```
function foo <tv>(params): R {
    expr
}
```

```
foo : <tv>(params) -> R
foo(params) = expr
```

Could introduce `do { decls }` as an expression to allow decls anywhere exprs are allowed.

Assignments without typesignature and without `fn` or `function` are not
generalized. Rationale: let should not be generalized. Fix for the user: add a typesig
or use `fn` syntax.

#### Switch directly on args, implicit switch

```
fn map(xs, f) {
    case Nil: Nil
    case Cons(x, xs): Cons(f(x), map(xs,f))
}
```

Automatically inserts a `switch` on the arguments.

Now `where` attaches to the branch, so `where:` needs to be used to span over
the entire (implicit) `switch`. And `where:` is available for explicit `switch` too.

### Operators

An identifier is either `[a-zA-Z_][a-zA-Z0-9_']*`
or operator `[?!@$#%^&|<>=-+*/]+`

Can operators be polymorphic? Problem to use <> with them

Do they need parens? At least `<>` will need because `(<>)<A>`

Possible: Identifiers with some spice `[@$!?a-zA-Z_][a-zA-Z0-9_']*[@$!?]?`

No operator sections.

### Types & typeclasses

Types are written in haskell-style without parens
Crocodiles `<`, `>` may be used for now, but could be repurposed for variants.

`function f <A, Eq A>(...): B { ... }`

Same as:

`function f <Eq A>(...): B { ... }`

`Eq` here has an argument which is not in scope, so it's introduced and put before
itself in `f`s type parameters.

- Flexible instances
- Flexible contexts
- Over rows, records, tuples (and effects?)

Q: Associated types without type equality?

Postpone MPTC

#### Local instances

Weird idea:

```typescript
class config {
    args: string[]
}
fn apa<config>(...) { ... }

fn main() {
    instance config {
        args: ["hello"]
    }
    apa() // now knows config
}
```

Not so terribly important because we have reader effect for this

### Tuples

Python one-element tuple solves syntax conflict `(T,)`

### GADTs

Postpone, but allow ExistentialQuantification.

### dot Dotsyntax

`f(a).g(b) = g(f(a), b)`

Fun consequence: `(f.g.h)(x) = h(g(f(x)))`

More expressive than record projections of classes since it may restrict
polymorphic fields of the class, i.e.

    conv : Store<Int> -> Store<Float>

now `x.conv()` only allowed if `x : Store<Int>`, this is not expressible as an OO class:

```java
class Store<U> {
    Store<Float> conv() {
        // want to restrict U = int somehow
    }
}
```

Difference from koka:
In koka `s.length` means `length(s)`, whereas in EffectScript it means `() => s.length()`
(for consistency)

### block function application

koka-style

```
if(a) { ds } { ds2 } = if(a, ds, ds2)
```

or

```
if(a) { ds } { ds2 } = if(a)(ds)(ds2)
```

Leaning towards the first: don't encourage too much curry

### polymorphic record projection

With brackets

    { [k]: e }

    s[k]

### where

An afterthought:

`{ ds1 where ds2 } = {ds2; ds1}`

Fun consequence: allows upside-down comprehensions

### Modules

`public`, `package`, `private` and possibly `protected` (down the hierarchy)

Agda-style `open`

### Doctests

This needs to be solved, here's a start

```typescript
module Apa
/**
@ambient
>>> import blabla // setup testing code

@before_each
>>> apa cepa

@after_each
>>> release?
*/

/*

>>> test() // run at compile-time, but not in documentation
apa

*/

/**
    >>> x = hello()
    "hello"
    >>> x + x // scope retained if not separated by blankline
    "hellohello"
*/
function hello(): string {
    "Hello"
}
```

### Handlers

Possible: separate effect pattern and resume contiunation with `;` or `!`
Possible: `return` or `done`

```typescript
handle i, s {
    case get(); r, s: r(s)
    case put(s); r, _: r(s)
    case return(a), s: (a, s)
}
```

```typescript
handle i {
    case get() ! r, s: r(s)
    case put(s) ! r, _: r(s)
    case return(a), s: (a, s)
}(s)
```


### Macros

We may go to macro world by `'` and `''` for list, and to expression world with
`[| ... |]`, `[t| ... |]`, `[e| ... |]`, `[p| ... |]`, `[d| ... |]`,
`[v| ... |]`, `[tv| ... |]`, etc.
or `branch '{ ... }`, `decl '{ ... }` etc

```typescript
fn Eq(A: Q<Type>) {
    decl '{
        instance Eq 'A extends ''ctx {
            fn ==(x,y) {
                ''cases
                default: false
            }
        }
    }
    where
    cs = A.constructors()
    ctx = map(A.typevars(), tv => type '{ Eq 'tv })
    cases = list {
        branch '{
            case: 'k(''kvs), 'k(''jvs): all('equal)
        }
        where
        c = each(cs)
        kvs = for(c.params()) { new_var }
        jvs = for(c.params()) { new_var }
        equal = list {
            expr '{ 'kv == 'jv }
            where
            kv, jv = each(zip(kvs, jvs))
        }
    }
}
```

The prio for this is low since:
    - It might not be useful for the compiler, and be too much work to work also in stage-0
    - TCs are less important than in Haskell

Possible: Use them as decorators
Possible: Add new (private/hidden) fields to types

```typescript
@Eq
data Maybe A {
  Nothing
  Just(A)
}

data Maybe A {
  Nothing
  Just(A)
  @Eq
}

data Maybe A {
  Nothing
  Just(A)
  expands Eq
}

data Maybe A {
  Nothing
  Just(A)
}
Eq(Maybe A)
Eq([t| Maybe A |])
```

### Record punning, unpacking constructors

```
    case C(..r):
    case D(..):
    case K(..{a, b}):
    case J(..{a, b, ..r}):
```

These work as expressions too:

    f(..r) // call f on args from r
    f(..) // call f with everything in scope (of which it needs)
          // f cannot have an open row type
    f(..{a, b}) // call f with a=a and b=b (not so useful, admittedly)
                // same as f(a=a, b=b)
    f(..{a, b, ..r}) // call f with a=a and b=b and rest as r
                     // same as f(a=a, b=b, ..r)

A few of these make sense with `*` for tuples

    case C(*r):
    case J(a, b, *r):
    f(*r)
    f(a, b, *r)

### Record update syntax

```
    // put
    { x => x + 1 }(r)
    r.{ x => x + 1 }

    // modify
    { x -> f }(r)
    r.{ x -> f }

    // modify
    { [x] -> f }(r)
    r.{ [x] -> f }

    // put on polymorphic key k
    { [x: k] => x + 1 }(r)
    r.{ [x: k] => x + 1 }

    // put and pattern-matching on static key x (conflict?)
    { [Pair(u, v): x] => u * v } : <R>{ x: Pair(int, int), R } -> { x: int, R }
```

### Record key comprehension

    applysome<Ks: Closed, Ks': Open, R: Rec Ks, S: Rec Ks', T: Rec (Ks + Ks')>
        ({[k in R]: R[k] -> S[k], T}, {R}) -> {S, T}

## Bootstrapping

Typeclasses:
Cheat with Eq and Ord and do structural equality except for hard-wired Map, Splay, Set.

Ord will be a bit off: order of constructors in data types not unambigous without types

Num & Monoid on primitives

Nick&hbmc taught us that it's easier to interpret than compile, even if the target looks similar to JS,
it isn't because of type-classes. Hence we should interpret the stage-0 compiler,
which will make it easy to call foreign code and hard-wire different kinds of hacks.

## To learn

Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism
