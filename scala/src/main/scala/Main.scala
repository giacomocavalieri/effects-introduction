import mtl.core.maybeDouble
import cats.Monad
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import mtl.stepper.Core.Stepper

object Main extends IOApp.Simple:
  def stepByStepProgram =
    import mtl.core.interpreters.StepperInterpreter.given
    import mtl.stepper.interpreters.IOStepper.given
    maybeDouble[IO](10)

  def productionProgram =
    import mtl.core.interpreters.Production.given
    maybeDouble[IO](10)

  val run =
    for
      _ <- IO.println("Run interactively? [y/n]")
      res <- IO.readLine >>= (_ match
        case "y" => stepByStepProgram
        case _   => productionProgram
      )
      _ <- IO.println(f"The result is: $res")
    yield ()
