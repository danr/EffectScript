I am starting to think that it makes sense to have row-typed/extensible variant types
in EffectScript. First I was skeptical because they are, as far as I know now,
useless together with recursion.

Now it's becoming apparent that there are many library combinators that make
sense with a nice variant/record symbiosis.

Examples:

  lexer

    lex<K, S: K -> *>(parsers: {[k in K]: Parse<S[k]>}): Parse<Variant<S>>

  oneof in quickcheck

    oneof<K: S: K -> *>(arbs: {[k in K]: Arbitrary<S[k]>}): Arbitrary<Variant<S>>

Much nice!
