+++
title = "Effects"
outputs = ["Reveal"]
langToDisplay = "scala"
+++


# Modellazione degli Effetti

---

## Effects

The essence of programming ultimately boils down to performing effects. As Simon Peyton Jones puts it:

 >If a program has no side-effects there's no point in running it, isn't it? You have a black box, you press go and it gets hot but there's no output <sup>[\^1](#resources)</sup>

---

## A simple running example

Being undoubtedly very convenient, many languages allow the programmer to use _statements_ as a way to perform unrestricted side-effects: I/O, failure, non-determinism, ...

```scala
def maybeDouble(n: Int): Int =
  println("Flipping a coin")
  val heads = util.Random.nextBoolean
  if heads then n*2 else n
```

---

The problem with unrestricted side effects is that there no longer is _referential transparency:_ while one can factor out duplicate values the same is not true for statements, they are not easily testable and it is overall harder to reason about the program's behavior

```scala
val program1: Int = maybeDouble(1) + maybeDouble(1)
val program2: Int = { val n = maybeDouble(1); n + n }
// the two programs are not equivalent since maybeDouble
// performs side effects
```

---

## Monads to the rescue

In a purely functional world the key difference is that a program no longer performs the side effects but it is an immutable data structure that _describes_ what side effects need to be performed:

{{< code scala >}}

```scala
def maybeDouble(n: Int): IO[Int] = for
  _     <- IO.println("Flipping a coin")
  heads <- IO(util.Random.nextBoolean)
yield (if heads then n*2 else n)
```

{{< /code >}}

{{< code haskell >}}

```haskell
maybeDouble :: Int -> IO Int
maybeDouble n = do
  putStrLn "Flipping a coin"
  heads <- randomIO
  pure $ if heads then n*2 else n
```

{{< /code >}}

---

Thanks to the `IO` monad one can treat _programs as first-class values:_

{{< code scala >}}

```scala
val p = maybeDouble(10)
val ns: IO[List[Int]] = List.fill(5)(p).sequence
```

{{< /code >}}

{{< code haskell >}}

```haskell
p :: IO Int
p = maybeDouble 10

ns :: IO [Int]
ns = sequence $ replicate 5 p
```

{{< /code >}}

`p` is not an integer value that could be either 10 or 20; it is an immutable data structure that describes the logic with which 10 may or may not be doubled by performing side-effects

---

## The problem with IO

Simply using `IO` does not magically solve our problems:

- the code inside `IO` is just as hard to test as the impure counterpart
- inside `IO` I could perform _all kinds_ of I/O (read a file, send messages over  
  the network, fail with an exception, ...)
- having the `IO` monad everywhere is like not having it at all!

---

## Goals of an effect system

We wish to express the side effects our code can perform in a way that:

- the code can be _easily composed and tested_
- the correct handling of side effects is _checked at compile-time_
- the side effects can be expressed with a _granularity suitable for our business needs_

---

Each effect system can be evaluated in terms of:

- cognitive load
- ease of use
- composability
- testability
- performance
- maturity
- ...

---

## Monad Stacks

Thanks to _monad transformers,_ monads can be _stacked_ together to obtain composite monads
that provide the side effects of each one of the monads composing the stack

{{< code scala >}}

```scala
// Enrich M[_] with the effects of the Maybe monad (failure)
final case class MaybeT[M[_], A](runMaybeT : M[Maybe[A]])
// Enrich M[_] with the effects of the State monad (global mutable state)
final case class StateT[S, M[_], A](runStateT : S => M[(A, S)])
// Enrich M[_] with the effects of the Reader monad (global read-only state) 
final case class ReaderT[S, M[_], A](runReaderT : S => M[A])
```

{{< /code >}}
{{< code haskell >}}

```haskell
newtype MaybeT    m a = MaybeT  { runMaybeT  :: m (Maybe a) }
newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s) }
newtype ReaderT s m a = ReaderT { runReaderT :: s -> m a }
```

{{< /code >}}

---

{{< code scala >}}

```scala
// With the compiler plugin -Ykind-projector:underscores enabled
// one can define a type lambda with the same syntax used for 
// a term lambda
def program: MaybeT[ReaderT[String, IO, _], Int] = ...
```

{{< /code >}}
{{< code haskell >}}

```haskell
program :: MaybeT (ReaderT String IO) Int
program = ...
```

{{< /code >}}

Using a monad stack allows expressing a program that can perform 3 kinds of side effects:
failure (`MaybeT`), reading a global immutable configuration (`ReaderT`) and performing I/O (`IO`)  

However, programming against a particular monad stack forces one to use a specific implementation
_breaking encapsulation_ <sup>[\^2](#resources)</sup>

---

## MTL / Tagless Final <sup>[\^3](#resources)</sup>

The effects/capabilities are encoded with type classes that can be
used as constraints to express the effects available in a given context

{{< code scala >}}

```scala
trait CoinFlip[F[_]] { def flipCoin: F[Boolean] }
trait Console[F[_]]  { def printLine(s: String): F[Unit] }

// Plus some more boilerplate...
def maybeDouble[M[_]: Monad: CoinFlip: Console](n: Int): M[Int] = for 
  _     <- Console[M].printLine("Flipping a coin")
  heads <- CoinFlip[M].flipCoin
yield (if heads then n*2 else n)
```

{{< /code >}}
{{< code haskell >}}

```haskell
class CoinFlip m where flipCoin :: m Bool
class Console  m where printLine :: String -> m ()

maybeDouble :: (Monad m, CoinFlip m, Console m) => Int -> m Int
maybeDouble n = do
  printLine "Flipping a coin"
  heads <- flipCoin
  pure $ if heads then n*2 else n
```

{{< /code >}}

---

## Interpreting the MTL program

Interpreting the program consists in choosing an appropriate monad
(inside which all effects can be carried out) and implementing
the instances of the needed type classes

{{< code scala >}}

```scala
given CoinFlip[IO] with
  override def flipCoin: IO[Boolean] = IO(util.Random.nextBoolean)

given Console[IO] with
  override def printLine(s: String): IO[Unit] = IO.println(s)

def main: IO[Unit] = for
  res <- maybeDouble[IO](10) // maybeDouble is interpreted as the IO monad
  _   <- IO.println(s"The result is $res")
yield ()
```

{{< /code >}}
{{< code haskell >}}

```haskell
instance CoinFlip IO where
  flipCoin :: IO Bool
  flipCoin = randomIO

instance Console IO where
  printLine :: IO ()
  printLine = putStrLn

main :: IO ()
main = do
  res <- maybeDouble 10
  putStrLn $ "The result is " <> show res
```

{{< /code >}}

---

## Free monads

The effects/capabilities are encoded as an ADT/GADT, every case must also
contain a field that represents the _continuation:_ a function that
takes an argument of a type appropriate to the effect being carried out
and returns a new value. For example, the continuation of the `FlipCoin` action takes a `Boolean`
value which represents the outcome of the coin flip

{{< code scala >}}

```scala
enum CoinFlipDSL[A] { case FlipCoin               extends CoinFlipDSL[Boolean] }
enum ConsoleDSL[A]  { case PrintLine(msg: String) extends ConsoleDSL[Unit] }
enum AppDSL[A] {
  case EvalCoinFlip[B](c: CoinFlip[B]) extends AppDSL[B] // GADT
  case EvalConsole[B](c: Console[B])   extends AppDSL[B]
}

// Plus some more boilerplate...
def maybeDouble(n: Int): App[Int] = for
  _     <- App.printLine("Flipping a coin!")
  heads <- App.flipCoin
yield (if heads then n * 2 else n)
```

{{< /code >}}
{{< code haskell >}}

```haskell
data CoinFlipDSL next = FlipCoin         (Bool -> next)
data ConsoleDSL  next = PrintLine String (()   -> next)
data AppDSL next
  = forall a. EvalCoinFlip (CoinFlip a) (a -> next)
  | forall a. EvalConsole  (Console a)  (a -> next)

-- Plus some more boilerplate...
maybeDouble :: Int -> App Int
maybeDouble n = do
  App.printLine "Flipping a coin!"
  heads <- App.flipCoin
  pure $ if heads then n * 2 else n
```

{{< /code >}}

---

## Interpreting the Free monad

Interpreting the program consists in providing a natural transformation
from the DSL to another language. There could be many interpreters, each one
_compiling the DSL to a lower-level instruction set_ that can then be interpreted
inside the `IO` monad

{{< code scala >}}

```scala
val productionInterpreter = new (AppDSL ~> IO):
  override def apply[A](a: AppDSL[A]): IO[A] = a match
    case EvalCoinFlip(c) => c.runWith(productionCoinFlipInterpreter)
    case EvalConsole(c)  => c.runWith(productionConsoleInterpreter)

val productionCoinFlipInterpreter = new (CoinFlipDSL ~> IO):
  override def apply[A](cf: CoinFlipDSL[A]): IO[A] = cf match
    case FlipCoin => IO(util.Random.nextBoolean)

val productionConsoleInterpreter = new (ConsoleDSL ~> IO):
  override def apply[A](c: ConsoleDSL[A]): IO[A] = c match
    case PrintLine(msg) => IO.println(msg)

def main = for
  res <- maybeDouble(10).runWith(productionInterpreter)
  _   <- IO.println(s"The result is: $res")
yield ()
```

{{< /code >}}
{{< code haskell >}}

```haskell
type f ~> g = forall a. f (g a) -> g a

appToIOInterpreter :: AppDSL ~> IO
appToIOInterpreter = \case
  EvalCoinFlip c k -> c `runWith` coinToIOInterpreter    >>= k
  EvalConsole  c k -> c `runWith` consoleToIOInterpreter >>= k

coinToIOInterpreter :: CoinFlipDSL ~> IO
coinToIOInterpreter (FlipCoin k) = randomIO >>= k

consoleToIOInterpreter :: ConsoleDSL ~> IO
consoleToIOInterpreter (PrintLine msg k) = putStrLn msg >>= k

main :: IO ()
main = do
  res <- maybeDouble 10 `runWith` appToIOInterpreter
  putStrLn $ "The result is: " <> show res
```

{{< /code >}}

---

## Ad hoc languages: Unison

Some languages give native support to algebraic effects: that is
functions are polymorphic on the effects they may produce and the type checker
verifies that there are no unhandled effects.

All the following examples are written in [Unison](https://www.unison-lang.org): a statically-typed, functional language which uses _abilities_ as a way to manage
effects

```haskell
structural ability CoinFlip where flipCoin : Boolean
structural ability Console  where printLine : Text -> ()

-- A function specifies in {} the abilities it needs, this function
-- requires the CoinFlip and Console abilities
maybeDouble : Nat ->  {CoinFlip, Console} Nat
maybeDouble n =
  printLine "Flipping a coin!"
  heads = flipCoin
  if heads then n*2 else n
  -- One can use a direct style, no monadic binding:
  -- if flipCoin then n*2 else n
```

---

## Interpreting a function with abilities: handlers

```ml
interpreters.runProductionApp : '{Console, CoinFlip} a ->{IO, Exception} a
  interpreters.runProductionApp comp =
    comp |> runConsoleIO |> runCoinFlipRandom |> lcg 11

interpreters.runConsoleIO : '{e, Console} a -> '{e, IO, Exception} a
interpreters.runConsoleIO comp _ =
  handler = cases
    {printLine msg -> k} ->
      console.printLine msg
      handle !k with handler
    { r }                -> r
  handle !comp with handler

interpreters.runCoinFlipRandom : '{e, CoinFlip} a -> '{e, Random} a
interpreters.runCoinFlipRandom comp _ =
  handler = cases
    {flipCoin -> k} -> handle k !Random.boolean with handler
    { r }           -> r
  handle !comp with handler
```

---

<!-- .slide: id="sources" -->
## Resources

1. [Simon P. Jones - Haskell is Useless](https://www.youtube.com/watch?v=iSmkqocn0oQ)
2. [Kammar, Lindley and Oury - Handlers in Action](http://dx.doi.org/10.1145/2500365.2500590)
3. [Mark P. Jones - Functional Programming with Overloading and Higher-Order Polymorphism](http://web.cecs.pdx.edu/~mpj/pubs/springschool95.pdf)
