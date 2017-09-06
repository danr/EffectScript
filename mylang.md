

Indentation makes for a good pre-pass of blocks to ignore blocks with syntax
errors (or type errors), and still process the rest of the file,
while at the same time making the code easy to read

if(b):
    ...
else if(j):
    ...
else:
    ...

when(b):
    ...
else:
    ...

for(xs) x:
    ...

try:
    ...
catch e:
    ...

while:
    cond
do:
    body

while(def: cond):
    body

while: cond do:
    ...

There is some wiggle-room here where to put parens, commas, semicolons and colons.
Idea: reserve colon in expression context for always introducing a new function.
(Except in type signature, ugh)

```python

def map(f: a -> <e> b, xs: List a) -> <e> List b:
    case xs of
        cons(y,ys) => cons(f(y), map(f, ys))
        nil => nil

def main(args: [string]) <io>:
    while(peek):
        ch = getchar()
        print(ch)

effect iter(a):
    yield(a)

effect ndet:
    choice() -> <ndet> bool

data 'iter a : Type -> Type:
    yield(a) -> 'iter a unit

data 'ndet : Type -> Type:
    choice() -> 'ndet bool


next: <e, f> a -> <e> (a + exists r . ('e r, r -> <e, f> a))

# or:
data 'iter a f b:
    yield(a, <iter a, f>)
    done(b)

data 'ndet f b
    choice(bool -> <ndet, f>)
    done(b)
    # put these in scope only as patterns
    # in expressions, put them in scope as effects

    # constructors & record labels can be ambiguous until
    # enough type information arrives
    #
    # should this be allowed for function calls, too?
    # (say they are defined in different modules)
    # maybe confusing if the same function name is used
    # at different types!
    # but it would be handy
    # maybe we can commit to one use of the function for
    # the entire module, hehe. not composable!
    # maybe issue a warning if a function is used at several types is enough.
    # idris has namespaces for this to disambiguate
    # the data constructor ambiguity is not composable either,
    # adding more data declarations might make code stop typechecking!
    # (or imports, of course)
    # but it is composable if there are type signatures
    # we could allow ambiguous constructors & records if they can
    # be disambiguated directly with the type context available
    # (same for functions?)

next: <e, f> a -> <e> (a + 'e f a)

def collect(p: <iter(a), e>) -> <e> list(a):
    case next(p) of
        yield(x, k) => x :: collect(k)
        _ => unit

maybe(a,n,def go(x): go(x+1))
if(b):
def peel(i : <iter a, e>) -> <iter a, e> Maybe a
    exec_state(Nothing, def:
        handle i of
            yield(a, resume) ->
                case get() of
                    Just(a0) -> yield(a)
                    Nothing  -> put(Just(a))
                resume()
    )

keywords: def, case, of, effect, data, class, instance, alias, import
maybe also: private, public, package(?), infix, infixr, infixl, prefix, suffix ?

public import ...    as, qualified, hiding, using, renaming (???)
package import ...

```

```python

delay(-> e) = ~ e
force(e)    = ! e

and(x, -> y)

while(p: -> e Bool, m: -> e .): e .

map(f: a -> e b) -> List a -> e (List b):
    xs -> case xs of
        y :: ys -> f(y) :: map(f, ys)
        []      -> []

map(f: a -> e b) -> List a -> e (List b):
    xs -> case xs of
        y :: ys -> f(y) :: map(f, ys)
        []      -> []

map(f: a -> e b, xs: List a) -> e (List b)
    case xs of
        y :: ys -> f(y) :: map(f, ys)
        []      -> []

    -- but where IS the effect type? can an expession have a type with an effect?
    -- it must be only on the functions

# using it
map(u -> case u of
        Nothing -> 0
        Just(y) -> y + 1, xs)
```

```python

def while(p: <e> Bool, m: () -> e ()) -> e ():
    when(p()):
        m()
        while(p,m)

def main(args: *string) io ():
    while(def(): peek(), def:
        ch = getchar()
        print(ch))

syntax(p,m) while p: m = while(def: p, def: m)
syntax(p,m) (m while p) = while(def: p, def: m)

def ifte(b: Bool, t: () -> e a, f: () -> e a) -> e a:
    case b:
        true: t()
        false: f()

syntax(b,t,f) if b: t; else: f = if(b,def (): t, def(): f)
infix syntax(b,t,f) (t if b else f) = if(b,def (): t, def(): f)

eff ndet:
    choice() -> Bool -- unary resume

-- CAFs returning functions same as lets in defs:
runNdet : (-> (ndet r, e) t) -> e (List t) =
    handle
        choice(resume): resume(false) + resume(true)
        return(x): singleton(x)

def runNdet(k : -> t <ndet r, e>) -> List t <e>:
    handle k:
        choice(b, resume): resume(false) + resume(true)
        return(x): singleton(x)

    -- confusing!

eff iter(a):
    yield(a) -- nullary resume

-- quadratic performance when peeling all elements from an iterator
-- incurs a performance penalty on every yield
-- note that you'll get the element /after/ all other elements :P
def peel(i : <iter a, e>) -> <iter a, e> Maybe a
    exec_state(Nothing, def:
        handle i of
            yield(a, resume) ->
                case get() of
                    Just(a0) -> yield(a)
                    Nothing  -> put(Just(a))
                resume()
    )

-- would need a shallow handler
def peel(i : <iter a, e>) -> <e> Tuple (Maybe a) <iter a, e>
    shallow i of
        yield(a, remaining) ->
            Tuple(Just(a), remaining)
        return(Unit) ->
            Tuple(Nothing, def: Unit)

def communicate(i : <coiter a b, e>, j : <iter a, e>) -> <iter b>
    p = handle j of
            yield(a, resume) -> send(a); resume()
            return(Unit) -> Unit
    handle p of

effect iter a:
    yield(a)

effect ear a:
    listen() -> <ear a> a  -- need parens if the result type spans several lines

def communicate(ear : <ear a, e>, mouth : <iter a, e>) -> <e> <iter a, e>:
    handle ear, mouth of
        return(_), m -> m
        listen(), m, resume ->
            shallow m of
                yield(a), m' -> resume(a, m')
                return(_) -> def: unit

effect parser tok:
    consume(maybe tok -> bool) -> <parser tok> (maybe tok)

def parse(s: list tok, m: <parser tok, e> a) -> <e> maybe a:
    handle m, s of
        return(a), _ -> just(a)
        consume(p), xs, continue ->
            case p(listToMaybe(xs)) of
                true -> continue(t, drop(1, xs))
                false -> nothing

effect ndet:
    choice() -> <ndet> bool

f : <ndet> x

run(f) : either (ndet {- as data -}, <ndet> x) -- hmm? shallow run

def each(m: <ndet, e> a) -> <e> seq a:
    handle m of
        choice(), resume -> resume(false) + resume(true)

def foreach(i: <iter a, e>, f: a -> <e> b) -> <e> seq b:
    each def:
        handle i of
            return(x) -> [x]
            yield(a), k ->
                case choice() of
                    false -> f(a)
                    true -> k()

data list a:
    cons(head : a, tail : list a)  -- name arguments because documentation... and selectors?
    nil()

elem : [Eq a](a, <iter a, e>) -> <e> bool = def (a, xs): or . map(a == _, _)

def or(i: <iter bool, e>) -> <e> bool
    handle i of
        yield(x), xs ->
            case b of
                true -> true
                false -> xs()
        return(_) -> false

def map(f: a -> <e> b, i: <iter a, e> r) -> <e> r:
    handle i of
        yield(a), xs -> yield(f(a)); xs()
        return(r) -> r

-- easy to fuse (or . map)?

def y'all(xs: list a) -> <iter a>
    case xs of
        nil -> ()
        cons(y, ys) -> yield(y); y'all(ys)

elem : [Eq a](a, list a) -> bool = or . map(a == _, _) . y'all

-- should be able to fuse this too
y'all: (xs: list a) -> <iter a> =
    fix $ \ loop ->          -- can apply TCO on y'all so we rewrite it with loop

    handle
        handle
            case xs of
                nil -> ()
                cons(y, ys) -> yield(y); loop(ys)
        of
            yield(a), xs -> yield(f(a)); xs()
            return(r) -> r
    of
        yield(x), xs ->
            case b of
                true -> true
                false -> xs()
        return(_) -> false
=
    fix $ \ loop ->
    handle
            case xs of
                nil -> ()
                cons(y, ys) ->
                    handle yield(y); loop(ys) of
                        yield(a), xs -> yield(f(a)); xs()
                        return(r) -> r
    of
        yield(x), xs ->
            case b of
                true -> true
                false -> xs()
        return(_) -> false
=
    fix $ \ loop ->
    handle
            case xs of
                nil -> ()
                cons(y, ys) -> yield(f(a)); loop(ys)
    of
        yield(x), xs ->
            case b of
                true -> true
                false -> xs()
        return(_) -> false
=
    fix $ \ loop ->
    case xs of
        nil ->
            handle () of
                yield(x), xs ->
                    case b of
                        true -> true
                        false -> xs()
                return(_) -> false
        cons(y, ys) ->
            handle yield(f(a)); loop(ys) of
                yield(x), xs ->
                    case b of
                        true -> true
                        false -> xs()
                return(_) -> false
=
    fix $ \ loop ->
    case xs of
        nil -> false
        cons(y, ys) ->
            case f(a) of
                true -> true
                false -> loop(ys)

-- easy to fuse (or . map)?


-- with associated types:

class Iterable f:
    type Elem f : *
    yall(f a) -> <iter (Elem a)>

elem : [Iterable f, Eq (Elem f)](Elem f, f) -> bool = or . map(a == _, _) . yall

elem : [Iterable f, Eq (Elem f)](Elem f, f) -> bool
elem = or . map(a == _, _) . yall
elem = def(x,xs): or(map(a == x, yall(xs)))
def elem(x,xs):
    or(map(a == x, yall(xs)))

# destructs to a constructor called elem
elem(x,xs) = ...

instance Iterable list a:
    type Elem (list a) = a
    ...

instance Iterable string:
    type Elem string = char
    ...

instance Iterable (Map k v):
    type Elem (Map k v) = Tuple k v
    ...

-- returns the remaining mouth
def communicate(ear : <ear a, e>, mouth : <iter a, e>) -> <e> <iter a>
    exec_state(mouth, def: -- oh yeah, Leijen has an explicit state arg here if necessary... hmm
        handle ear of
            return(unit) -> unit
            listen(resume) ->
                shallow get() of
                    yield(a, remaining)
                        put(remaining)
                        resume(a)
                    return(unit) ->
                        put(def: unit)
                        unit)

def all(i: <iter bool, e>) -> Bool <e>
    handle i of
        yield(b, resume):
            case b of
                true: resume()
                false: false
        return(_) -> true

-- x && y = all(def: yield(x); yield(y))

unitUnit : <>
unitUnit = def (unit): unit

def forM(i: -> r <iter a, e>) -> <ndet r, e>:
    handle i of
        yield(a,resume):
            case choice():
                false: a
                true: resume()

def for_(i: () -> (iter a, e) r, k: a -> e ()) -> e r:
    handle i of
        yield(a): k(a); resume()

syntax(xs,m(x)) for x in xs: m = for(def: xs, def(x): m)
infix syntax(xs,m(x)) (m for x in xs) = for(def: xs, def(x): m)

infixr 8 syntax(l,r) (l && r) = conj(l, def: r)

syntax(e,n,m(x,xs)) unlist e n x xs: m =
    case e of
        cons(x,xs): m
        nil: n
```



# wish: extensible data types
# or restrictable... is that a row type?
# row types for constructors?!

kind expr = boolean => Type # lol

data expr(simple: boolean):
    add(a: expr(s1), b: expr(s2)) -> expr(s1 && s2)
    lit(int) -> expr(simple)
    hole(string) -> expr(false)
    syntax_error(string) -> expr(false)

data simple_expr restricts expr:
    add(simple_expr, simple_expr)
    lit(int)

record pair(a,b):
    fst : a
    snd : b

data pair(a,b):
    record pair:
        fst : a
        snd : b

pair(fst=a, snd=b)

pair(p_..) = p        # !!!???!!!
q_fst = p_snd
q_snd = p_fst
pair(q_..)     # lol record wild cards + renaming, LOL

# what about?
record pair:
    fst = p.snd
    snd = p.fst

but isn't this the same as:

fst = p.snd
snd = p.fst
pair(..)

but less "safe" for accidentally capturing some other field?


but it would be nice to have a general way to call functions
that have named parameters, and not use () and ,

record wildcards are probably the best way to do this...

Config(.., debug=True)

hmm... first I make a big point about having fixed arities,
then I do a huge fuzz to try to hide all parens and commas :p

data pair(a,b):
    pair(fst: a, snd: b)

subst : (string -> expr(a)) -> expr(a) -> expr(a)

restrict : expr(false) -> <exn(expr(false))> expr(true)

f : expr(true) -> expr(true)
f : expr(a) -> expr(a)

f : expr -> expr
f : simple_expr -> simple_expr # if this can be proven

# the tightest function is always chosen... umm

