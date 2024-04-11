package part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.{Fiber, IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object RacingIOs extends IOApp.Simple {

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"starting computation: $value").myDebug >>
        IO.sleep(duration) >>
        IO(s"computation for $value done") >>
        IO(value)
      ).onCancel(IO(s"computation canceled for $value").myDebug.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.seconds)
    val favLang = runWithSleep("scala", 2.seconds)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    //val first: IO[Either[Int, String]] = unrace(meaningOfLife, favLang)
    /*
   - both IOs run on separate fibers
   - the first one to finish will complete the result
   - the loser will be canceled
  */
    first.flatMap {
      case Left(value) => IO(s"Meaning of Life won: $value")
      case Right(value) => IO(s"Fav language won: $value")
    }
  }

  def testRaceWithPair(): IO[Outcome[IO, Throwable, _ >: Int with String]] = {
    val meaningOfLife = runWithSleep(42, 1.seconds)
    val favLang = runWithSleep("scala", 2.seconds)

    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String]) // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang)) => fibLang.cancel >> IO("MOL won").myDebug >> IO(outMol).myDebug
      case Right((fibMol, outLang)) => fibMol.cancel >> IO("Language won").myDebug >> IO(outLang).myDebug
    }
  }

  /*
     Exercise
     1. implement a timeout pattern with race
     2. a method to return a Losing effect from a race (hint: use racePair)
     3. implement race in terms of racePair
   */

  // 1
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val timeoutEffect = IO.sleep(duration)
    val result = IO.race(io, timeoutEffect)

    result.flatMap {
      case Left(value) => IO(value)
      case Right(_) => IO.raiseError(new RuntimeException("Computation timed out"))
    }
  }

  private val importTask = IO.sleep(2.seconds) >> IO(42)
  val testTimeout = timeout(importTask, 3.seconds)
  val testTimeout_v2 = importTask.timeout(1.seconds)

  // 2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB)) => fibB.join.flatMap {
        case Outcome.Succeeded(fa) => fa.map(Right(_))
        case Outcome.Errored(e) => IO.raiseError(e)
        case Outcome.Canceled() => IO.raiseError(new RuntimeException("Loser cancelled"))
      }
      case Right((fibA, _ )) => fibA.join.flatMap {
        case Outcome.Succeeded(fa) => fa.map(Left(_))
        case Outcome.Errored(e) => IO.raiseError(e)
        case Outcome.Canceled() => IO.raiseError(new RuntimeException("Loser cancelled"))
      }
    }
  }

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => outA match {
        case Outcome.Succeeded(fa) => fibB.cancel >> fa.map(Left(_))
        case Outcome.Errored(e) => fibB.cancel >> IO.raiseError(e)
        case Outcome.Canceled() => fibB.join.flatMap {
          case Outcome.Succeeded(fa) => fa.map(Right(_))
          case Outcome.Errored(e) => IO.raiseError(e)
          case Outcome.Canceled() => IO.raiseError(new RuntimeException("Both computation cancelled."))
        }
      }
      case Right((fibA, outB)) => outB match {
        case Outcome.Succeeded(fb) => fibA.cancel >> fb.map(Right(_))
        case Outcome.Errored(e) => fibA.cancel >> IO.raiseError(e)
        case Outcome.Canceled() => fibA.join.flatMap {
          case Outcome.Succeeded(fa) => fa.map(Left(_))
          case Outcome.Errored(e) => IO.raiseError(e)
          case Outcome.Canceled() => IO.raiseError(new RuntimeException("Both computation cancelled."))
        }
      }
    }

  override def run: IO[Unit] =  {
    //testRaceWithPair().void
    // timeout(importTask, 3.seconds).myDebug.void
    testRace().myDebug.void
  }
}