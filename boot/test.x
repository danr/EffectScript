
type list<a> {
    nil
    cons(head: a, tail: list<a>)
}

g : () -> unit ! yield<int>
g : unit ! yield<int>
g : ! yield<int>

function map<A,B,E>(f: A -> B ! E, xs: list<A>): list<B> ! E {
  switch xs {
    case nil: nil
    case cons(x, xs): cons(f(x), map(f, xs))
  }
}

map(x => { modify(st => st + x) x + 1 }, cons(1, nil))

function apa<A,B,C>(x: int, z: string ! a ! b ! c) {
    (y => y)(1)
    f(a, z => z)(2)
    f((a, z) => z)(2)
    switch {
        case y:
            true
    }
    switch a, b, c {
        case u, v, w, true = apa, cons(u,v) = h("malin"):
            lol
        case _:
            hehe
    }
    apa = h("dannen")
    branch(left,elem,right) = h("dannen")
    t = x => x
    u = x => {
        x * x
    }
    t = x => x : z -> z // shift/reduce/conflict if next expr starts with (
    function z( x ) {
      "str"
    }
}



z(12, 34)("malin", "dan")

(switch x { case A: f; case B: g } < h)(x)













