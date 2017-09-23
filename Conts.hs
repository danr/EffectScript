

effect Email<A> {
  Send(x : Int): Bool
  Done(a : A): 0
}

SendData(x : Int, k : Bool -> A ! Email<A>) : EmailData<A>
DoneData(a : A, k : 0 -> A ! Email<A>) : EmailData<A>

function Inbox(nick : () -> 0 ! Email<String>) {
  switch next(nick) {
    case Send(x, k): Inbox({k(x > 100)}); puts("Email processed!")
    case Done(msg, k): puts("Last message (not an email): " + msg)
  }
}

function Inbox(nick : () -> 0 ! Email<String>) {
  u = next(nick)
  switch u {
    case Send(x, k):
      v1 = Inbox({k(x > 100)});
      puts("Email processed!")
    case Done(msg, k):
      puts("Last message (not an email): " + msg)
  }
}

Inbox({
  do Send(5)
  do Send(1005)
  do Done("HELLO?")
})





effect Error {
  raise<B>(msg : String): B
}

function try(p : () -> R ! Error): Either<String, R> {
  switch next(p) {
    case raise(msg, _k): Left(msg)
    case done(r): Right(r)
  }
}


effect Done<A> {
  done(a : A): Void
}

runNDet({runIter({ yield(1); yield(if boolean() then 2 else 3); "nick" })})

function runNDet(p) {
    switch next(p) {
        case boolean(k): runNDet({k(true)}) ++ runNDet({k(false)})
        case done(s): [s]
    }
}

function runNDet(p) {
    switch next({let* x = p() in DONE_NDET(x)}) {
        case boolean(k): runNDet({k(true)}) ++ runNDet({k(false)})
        case DONE_NDET(s, k): [s]
    }
}

function runIter(p)
    switch next(p : () -> String ! Yields<Int> ) {
        case yield(i, k): printInt(i); runIter({k()}); printString("returning...")
        case done(s, k : Void -> ...): printString(s)
    }
}

data Effect = Effect Label [Value] (Value -> Value)

data Data = EffectData Effect | DoneData Value

type Handlers = (Effect {- example: yield/done -} -> ... -> Value)


go :: Env -> Expr -> Handlers -> (Handlers -> Value -> Value) -> Value
go env (App e1 e2) h k =
  go env e1 h $ \ h2 v1 ->
  go env e2 h2 $ \ h3 v2 ->
  case v1 of
    Fun x closure rhs ->
      go (M.insert x v2 closure) rhs h3 k
go env (e1; e2) h k =
  go env e1 h $ \ h2 UnitValue ->
  go env e2 h2 $ \ h3 v2 ->
  k h3 v2
go env (Op label vs) h k = h (Effect label vs k)
go env (Next labels e) h0 k0 =
  go env e h0 $ \ h2 v ->
  case v of
    Fun [] closure rhs ->
      let loop h k =
            go closure rhs
              (\ effect@Effect{label} ->
                    if label `elem` labels then
                      k h2 (EffectData effect)
                    else
                      h2 effect $ \ h4 k4 effect ->
              )
              (\ h3 v -> k h3 (DoneData v))
      in loop h2 k0




go [] (Next ({(yield(1); "nick")})) (error "unhandled!") (\ h v -> caseOnData v)

go [] (yield(1); "nick") (\ (YieldEffect i k2) -> (\ h v -> caseOnData v) (error "unhandled!") (YieldData i k2))
                         (\ h3 v -> (\ h v -> caseOnData v) (error "unhandled") (DoneData v))

go [] (yield(1); "nick") (\ (YieldEffect i k2) -> caseOnData (YieldData i k2))
                         (\ h3 v -> caseOnData (DoneData v))




--
go [] (yield(1); "nick") (H = (\ (YieldEffect i k2) -> return (YieldData i k2)))
                         (K = (\ h3 v -> return (DoneData v)))

go [] (yield(1); "nick") H K

go [] yield(1) H $ \ h v -> go [] "nick" h $ \ h2 v2 -> K h2 v2

go [] yield(1) H $ \ h v -> go [] "nick" K

H (YieldEffect 1 (\ h v -> go [] "nick" K))

return (YieldData 1 (\ h v -> go [] "nick" K))

--
