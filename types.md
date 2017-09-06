
EffectScript
extension: .esc

$ esc --help
EffectScript compiler (esc) v0.1


Row polymorphism for effects does not require
absence or subtraction since it's ok to make a type
more general.
The polarities are reversed: you cannot call
a function if you have too many effects, whereas
it would be OK with records (the reciever won't
be able to project them, and you'll forget
that the fields are there. For effects the opposite
is desired: you must not forget what effects you
have but you can always claim you have more effects).


Tuples & varargs/rest params/splats with row types + typeclasses should be OK
But how does varargs interact with (default) named parameters?
With supplying functions that don't look at all their arguments?


Duplicate labels with shadowing is what you want.
Might get some function f : () -> <e> int and you
want to run it with more effects:

    writer {
      thrice {
        tell(f())
      }
    }

Here if the polymorphic effect <e> also contains some
writer<int>, you don't want to capture those! They
are handled by some other handler further up the chain.
So there is the scoping. It's totally correct.

----

Generator example

    quick_check(g : () -> gen a, prop: a -> <exn> bool): <console, rnd> bool

Here we don't want to allow more effects neither in the generator nor in the property.
But this fact won't really be exploited in the implementation of quick_check, right?
So it's really only help from the library creator.
But we can make some guarantees, like that the generator will produce the same
value on the same starting seed, which we couldn't do if there were more effects
involved. But that only matters from a reasoning perspective. But: in QC we could
have a GenT and hope noone ever uses it :P

But GenT would only morally satisfy these because of splitting the seed

    lift f >>= lift g = lift (f >>= g)
    lift return = return

---

Rust's traits are oriented towards one self, so you can write

function elem<A: Eq>(x: A, xs: A[]): boolean {
    ...
}

It's pretty nice.

But elem<A, Eq A>(...) is ok too and is less confusing for Haskell programmers :P

Actually, A: Eq could just be sugar for that.

<A: Eq + Ord, B: ...>

It makes some sense to have type class contexts in the <...>

(But what should the syntax for effect rows be? That cannot be <..> /too/! But Koka has it that way)

NB: Rust also has multi-param classes (perhaps with automatic fundeps, it's a bit hard to tell)
and associated types! But higher-kinded classes are either not there or must be encoded as of now.

----

Tail call optimization when returning constructors is OK!
Tail recursive modulo cons
(if there are effects the call needs to be rightmost)

Same if the expression is known to be a monoid (+, *, &, |)

f(x) = E + f(x-1) ~> f(x) = f(x,0); f(x,acc) = f(x-1,acc + E)
f(0) = B          ~> acc + B

f(2) = E2 + f(x-1) = E2 + (E1 + B)

f(2) = f(2,0) = f(1,0 + E2) = f(0,(0 + E2) + E1) = ((0 + E2) + E1) + B

(complexity should also be associative: consider ++)

----

Can reorder computations when there are no effects: if by no
effects we also mean that there cannot be exceptions nor divergence.
A little termination checker could be good here. Walter recursion?


---
shallow handlers a little messier? consider coinflip:

```koka
effect amb {
  fun flip() : bool
}

val coinflip = handler {
  flip() -> resume(random-bool())
}
```

function coinflip(p: a!amb e): a!rnd e {
  switch next(p) {
    case done(x): x
    case flip(k): b=random() coinflip{k(b)}
  }
}
// longer but... whatever?
// type of k could contain more continuations and allow us to write this:

    case flip(k): coinflip(k(random()))

// or if we had $ : ((a, *as) -> b!e) -> a -> *as -> b!e

    case flip(k): coinflip(k $ random())

// they are still more straight-forward. I don't really like the magic resume

----

itermap(i: !iter<a>, f: a -> b!e) -> !iter<b> e

zero = { yield(0) } : !iter<int>

```
!@#$%^&*()+~?-/\[]|{}_`~=
```
compose : (f: b -> c!e, g: a -> b!e) -> a -> c!exn io partial rnd e
                                                               -- ^ how do we know this isn't the first token on the next line?
                                                               -- a little tricky to parse this id list since it might collide with the next line
                                                               -- needs one token of lookahead :(

Ok to call a function with less effects

    compose : (b -> e c, a -> e b) -> a -> e c

    compose(print : int -> console (), round: double -> () int): double -> console ()

    ok to consider
        round: double -> console int
    because eta expanding to
        x => round(x)
    can get that type

This makes code with polymorphic effects like upper bounds

What about a stale id, without effects?

    id : a -> () a

Can we now call this?

    id(print('hej'))

We should, since the type of print('hej') is () | console (unit but creates effects of type console)

Why should id have some other type?

Depends on the typing rule for application?

    f: a -> e b; x: a | e'
    ----------------------
        f x: b | e + e'
Annoying to join types so easier to have this:
    f: a -> e b; x: a | e
    ----------------------
        f x: b | e

or this:
    f: a -> e1 b;  x: a | e2;  e1 < e;  e2 < e
    ------------------------------------------
                 f x: b | e

Or if f is an expression:
    f: a -> e1 b | e0;  x: a | e2;  e0 < e; e1 < e;  e2 < e
    -------------------------------------------------------
                         f x: b | e

I guess there's the design space:
    - open/close (explicit (ugh) or implicit)
        - maybe simple to open at every function call ?
          this will make all types match up at the upper bound
    - subtyping or union in function application

TODO: Check LAM and next rules

LAM should be:

     G, x : a |- b : r | e
    --------------------------------
     G |- (\ x -> b) : a -> e r | ()

or:

     G, x : a |- b : r | e1;  e1 < e
    --------------------------------
     G |- (\ x -> b) : a -> e r | ()

(producing a function makes no effect: no evaluation under lambda)
which means we can pick an arbitrary effect:

     G, x : a |- b : r | e
    --------------------------------
     G |- (\ x -> b) : a -> e r | e'

- next (expressed as a primive built-in function:)

    G |- next : (() -> <l, e> a) -> e ('l e a) | e'

    effect l where
        op(t): <l> r

    data 'l e a where
        op(t, r -> <l, e> a)    // note: r is a concrete type, not existential
        done(a)


// then this should be ok:
function imap(f: <yield a, e> r, g: a -> <yield b, e> b): <yield b, e> r {
    switch next(f) {
        case done(r): r
        case yield(a, k):
            yield(g(a))
            imap(k, g)
    }
}


Only with HOFs is it interesting

    id2 : (a -> () b) -> a -> () b

Now we cannot call id2(print)('hej') since print does not have an empty effect

# What is the point of tracking effects?

- make sure that every effect gets handled

- expose functions as pure as possible

- decreas einteraction of impure/global program parts

(it's a like tracking exception which is pretty annoying.
but it's also like tracking side-effects with monads which is OK)

# iterators

    // should be ok
    itermap1(f: () -> (iter a, e) r, g: a -> e b): (iter b, e) r

    // should this be ok?
    itermap2(f: () -> (iter a, e) r, g: a -> (iter b,e) b): (iter b, e) r

If one calls itermap1 with a (g: a -> (iter b,e) b), you'd end up with a (iter b, iter b, e) r.
> Which is weird! Which one of the effects does the yield go to now?
> Should either disallow this or flatten them

# Question: what about multiple labels of the same type?

# Question: labels with same head but different arguments

# Open rule:
Can open a closed row by using a polymorphic tail.
This is essentialy the same as eta expansion (since a lambda gets to choose a new effect)
- Could make this explicit that way.

# Close rule:
If some code is polymorphic in effects, it's ok to close it
- Could close it explicitly with a lambda


close:
    f: a -> e b
    (a => f(a)) : a -> () b

open:
    g: a -> K b
    (a => g(a)) : a -> (K + e) b

(should these happen automatically?)

# Example: STM. Very important no effects happen here

    atomically : (() -> stm a) -> io a

    atomically({
        ... stuff ...
    })

(not polymorphic in effects)
You could do effects inside the atomically block,
but you'd have to discharge them inside there as well!
(Just like you can have ReaderT STM, but there is no STMT)

    runHeap : (forall h. () -> heap<h> a) -> a

this could also be without polymorphism, but it doesn't have to (?)

But are these "very important" handlers only built-in?
How can we inside the language leverage that we are the only effect
or that there are no effects being made?
