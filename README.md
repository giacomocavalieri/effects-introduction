# effects-introduction
![Haskell CI status](https://github.com/giacomocavalieri/effects-introduction/actions/workflows/haskell-build.yaml/badge.svg)
![Scala CI status](https://github.com/giacomocavalieri/effects-introduction/actions/workflows/scala-build.yaml/badge.svg)

The project aims to get an overview of some of the most prominent (or emerging) effect systems:

- _mtl/tagless final_
- _free monads_
- _ad hoc languages_

## Project's structure
The project starts from a rather dumb function that performs an effectful computation; in plain Scala:

```scala
def maybeDouble(n: Int): Int =
  println("Flipping a coin!")
  val heads = util.Random.nextBoolean
  if head then n*2 else n
```

Then, for each effect system it shows:
- how to encode the function in the effect system
- how the core logic of the resulting code can be tested
- how the program could be interpreted in different ways

All the examples are written in Haskell, Scala and Unison: the projects have almost exactly the same
structure so that if you know one of the languages it (hopefully) should not be too hard to follow the
examples in the other.

## Running the examples

|                             | Scala      | Haskell             | Unison (in `ucm`)   |
|-----------------------------|------------|---------------------|---------------------|
| run the interactive example | `sbt run`  | `cabal run effects` | `run coinFlip.main` |
| run the tests               | `sbt test` | `cabal test`        | `test`              |

## A note on the Unison code

[Unison](https://www.unison-lang.org) is a content-addressed language and stores its code in a format that
GitHub can not currently show; that's why for now in the repository I included a simple `code.u` file with
all thee definitions that would be stored in the codebase. It's just to make it easier to keep everything
in one place. However, I highly recommend to browse the Unison code directly from its
[repository on Unison Share](https://share.unison-lang.org/@giacomocavalieri/code/latest/namespaces/public/learning/thesis/coinFlip)
