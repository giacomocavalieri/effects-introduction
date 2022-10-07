import cats.Monad
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import mtl.core.maybeDouble
import mtl.stepper.Core.Stepper
import free.core.maybeDouble as freeMaybeDouble
import free.core.*
import free.core.interpreters.Production.productionInterpreter
import free.core.interpreters.Stepper.appToStepperInterpreter
import free.stepper.interpreters.IOStepper.stepperToIOInterpreter

object Main extends IOApp.Simple:
  override def run =
    val msg = "1. run the MTL example\n2. run the Free example\nChoose one of"
    for
      res <- choose(msg)("1", "2")(MTL.mainMTL, Free.mainFree)
      _ <- IO.println(s"The result is: $res")
    yield ()

object MTL:
  val stepByStepProgram =
    import mtl.core.interpreters.StepperInterpreter.given
    import mtl.stepper.interpreters.IOStepper.given
    maybeDouble[IO](10)
  val productionProgram =
    import mtl.core.interpreters.Production.given
    maybeDouble[IO](10)
  val mainMTL = chooseYN("Run interactively?")(stepByStepProgram, productionProgram)

object Free:
  val stepByStepProgram =
    free.core.maybeDouble(10).runWith(appToStepperInterpreter).runWith(stepperToIOInterpreter)
  val productionProgram = free.core.maybeDouble(10).runWith(productionInterpreter)
  val mainFree = chooseYN("Run interactively?")(stepByStepProgram, productionProgram)

def choose[A](msg: String)(opta: String, optb: String)(a: IO[A], b: IO[A]): IO[A] =
  for
    _ <- IO.println(f"$msg [$opta/$optb]")
    answer <- IO.readLine
    res <- answer match
      case s if s == opta => a
      case s if s == optb => b
      case _              => choose(msg)(opta, optb)(a, b)
  yield res

def chooseYN[A](msg: String)(a: IO[A], b: IO[A]): IO[A] = choose(msg)("y", "n")(a, b)
