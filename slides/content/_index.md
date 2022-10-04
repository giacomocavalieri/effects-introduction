+++
title = "Effects"
outputs = ["Reveal"]
+++


# Modellazione degli Effetti

---

## Effects

The essence of programming ultimately boils down to performing effects. As Simon Peyton Jones puts it:

 >If a program has no side-effects there's no point in running it, isn't it? You have a black box, you press go and it gets hot but there's no output [\^1](#resources)

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

The problem with unrestricted side-effects is that there no longer is _referential transparency:_ while one can factor out duplicate values the same is not true for statements, they are not easily testable and it is overall harder to reason about the program's behavior

```scala
val program1: Int = maybeDouble(1) + maybeDouble(1)
val program2: Int = { val n = maybeDouble(1); n + n }
// the two programs are not equivalent since maybeDouble
// performs side effects
```

---

## Monads to the rescue

In a purely functional world the key difference is that a program no longer performs the side effects but it is an immutable data structure that _describes_ what side effects need to be performed:

```scala
def maybeDouble(n: Int): IO[Int] = for
  _     <- IO.println("Flipping a coin")
  heads <- IO(util.Random.nextBoolean)
yield (if heads then n*2 else n)
```

```haskell
maybeDouble :: Int -> IO Int
maybeDouble n = do
  putStrLn "Flipping a coin"
  heads <- randomIO
  pure $ if heads then n*2 else n
```

---

Thanks to the `IO` monad one can treat _programs as first-class values:_

```scala
val p = maybeDouble(10)
val ns: IO[List[Int]] = List.fill(5)(p).sequence
```

`p` is not an integer value that could be either 10 or 20; it is an immutable data structure that describes the logic with which 10 may or may not be doubled by performing side-effects

---

Monads can capture the idea of sequencing a wide range of side effects:

- `State` mutable global state
- `Maybe` possible failure with short-circuiting
- `Reader` global immutable state
- `List` non-determinism
- `Future, Writer, Parser, ...`

---

## Dealing with multiple side-effects

However our code usually needs more than a single side-effect, luckily, thanks to _monad transformers_ monads can be _stacked_ together to obtain composite monads

```scala
final case class MaybeT[M[_], A](runMaybeT : M[Maybe[A]])
final case class StateT[S, M[_], A](runStateT : S => M[(A, S)])
final case class ReaderT[S, M[_], A](runReaderT : S => M[A])
```

```haskell
newtype MaybeT    m a = MaybeT  { runMaybeT  :: m (Maybe a) }
newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s) }
newtype ReaderT s m a = ReaderT { runReaderT :: s -> m a }
```

---

```scala
// With the compiler plugin -Ykind-projector:underscores enabled
// one can define a type lambda with the same syntax used for 
// a term lambda
def program: MaybeT[ReaderT[String, IO, _], Int] = ...
```

```haskell
program :: MaybeT (ReaderT String IO) Int
program = ...
```

`program` can perform 3 kinds of side-effects:

- it can fail (`MaybeT`)
- it can read a global configuration of type `String` (`ReaderT`)
- it can perform I/O (`IO`)

---

There still are problems: the code inside `IO` is just as hard to test as the impure counterpart; the only way to get a result out of `IO` is to interpret the data structure ultimately performing the side effects

((TODO Esempio di codice))
((TODO trovare articolo in cui si parla di come esplicitare la monade in questo modo rompe incapsulamente, program against an interface))

---

## MTL / Tagless Final

```scala
def maybeDouble[M[_]: Monad: CoinFlip: Console](n: Int): M[Int] =
  for 
    _     <- Console[M].println("Flipping a coin")
    heads <- CoinFlip[M].flipCoin
  yield (if heads then n*2 else n)
```

```haskell
maybeDouble :: (Monad m, CoinFlip m, Console m) => Int -> m Int
maybeDouble n = do
  println "Flipping a coin"
  heads <- flipCoin
  pure $ if heads then n*2 else n
```

---

## Free monads

```scala
def maybeDouble(n: Int): App[Int] =
  for
    _     <- print("Flipping a coin")
    heads <- flipCoin
  yield (if heads then n*2 else n)
```

```haskell
maybeDouble :: Int -> App Int
maybeDouble n = do
  println "Flipping a coin"
  heads <- flipCoin
  pure $ if heads then n*2 else n
```

---

## Unison

```haskell
maybeDouble : Int -> {CoinFlip, Console} Int
maybeDouble n =
  println "Flipping a coin"
  heads = flipCoin
  if heads then n*2 else n
  -- One can use a direct style, no monadic binding!
  -- if flipCoin then n*2 else n
```

---

<!-- .slide: id="sources" -->
## Resources

1. [Simon Peyton Jones - Haskell is Useless](https://www.youtube.com/watch?v=iSmkqocn0oQ) ^1
