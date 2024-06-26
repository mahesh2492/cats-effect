package part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Fibers extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("Scala")

  def sameThreadIOs() = for {
    _ <- meaningOfLife.myDebug
    _ <- favLang.myDebug
  } yield ()

  def createFiber: Fiber[IO, Throwable, String] = ??? //almost impossible to create fibers manually

  // the fiber is not actually started, but the fiber allocation is wrapped in another effect
  val aFiber = meaningOfLife.myDebug.start

  def differentThreadsIO(): IO[Unit] = for {
    _ <- aFiber
    _ <- favLang.myDebug
  } yield ()

  //joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]) = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result
  /*
  possible outcomes:
  - success with an IO
  - failure with an exception
  - cancelled
  */

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread: IO[Int] = someIOOnAnotherThread.flatMap {
    case Outcome.Succeeded(effect) => effect
    case Outcome.Errored(e) => IO(0)
    case Outcome.Canceled() => IO(0)
  }

  def throwOnAnotherThread(): IO[Outcome[IO, Throwable, Int]] = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel(): IO[Outcome[IO, Throwable, String]] = {
    val task = IO("starting").myDebug >> IO.sleep(1.seconds) >>IO("done").myDebug
    // onCancel is a "finalizer", allowing you to free up resources in case you get canceled
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelled.").myDebug.void)

    for {
      fib <- taskWithCancellationHandler.start //on a separate thread
      _ <- IO.sleep(500.milli) >> IO("cancelling").myDebug // running on calling thread
      _ <-fib.cancel
      result <- fib.join
    } yield result
  }

  /**
   * Exercises:
   * 1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
   * - return the result in an IO
   * - if errored or cancelled, return a failed IO
   *
   * 2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results.
   * - if both IOs complete successfully, tuple their results
   * - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
   * - if the first IO doesn't error but second IO returns an error, raise that error
   * - if one (or both) canceled, raise a RuntimeException
   *
   * 3. Write a function that adds a timeout to an IO:
   * - IO runs on a fiber
   * - if the timeout duration passes, then the fiber is canceled
   * - the method returns an IO[A] which contains
   * - the original value if the computation is successful before the timeout signal
   * - the exception if the computation is failed before the timeout signal
   * - a RuntimeException if it times out (i.e. cancelled by the timeout)
   */
  //1
  def processResultFromFiber[A](io: IO[A]): IO[A] = {
    val ioResult = for {
      fib <- io.start
      result <- fib.join
    } yield result

    ioResult flatMap {
      case Outcome.Succeeded(effect) => effect
      case Outcome.Canceled() => IO.raiseError(new RuntimeException("cancelled"))
      case Outcome.Errored(e) => IO.raiseError(e)
    }
  }

  def testExercise1() = {
    val aComputation = IO("starting").myDebug >> IO.sleep(1.seconds) >> IO("done").myDebug >> IO(42)
    processResultFromFiber(aComputation).myDebug.void
  }

  //2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val result = for {
      fiba <- ioa.start
      fibb <- iob.start
      resulta <- fiba.join
      resultb <- fibb.join
    } yield (resulta, resultb)
    result.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => for {
        a <- fa
        b <- fb
      } yield (a, b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(new RuntimeException("some computation canceled."))
    }
  }

  def testExercise2() = {
    val firstIO = IO.sleep(2.seconds) >> IO(1).myDebug
    val secondIO = IO.sleep(3.seconds) >> IO(2).myDebug
    tupleIOs(firstIO, secondIO).myDebug.void
  }

  //3
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computation = for {
      fib <- io.start
      _ <- (IO.sleep(duration) >> fib.cancel).start // careful - fiber can leak
      result <- fib.join
    } yield result
    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Outcome.Canceled() => IO.raiseError(new RuntimeException("Computation cancelled"))
    }
  }

  def testExercise3() = {
    val aComputation = IO("starting").myDebug >> IO.sleep(1.seconds) >> IO("done").myDebug >> IO(42)
    timeout(aComputation, 500.millis).myDebug.void
  }

  override def run: IO[Unit] = {
    runOnSomeOtherThread(meaningOfLife) //IO(Succeeded(IO(42))
      .myDebug
      .void

    someResultFromAnotherThread.myDebug.void
    throwOnAnotherThread().myDebug.void

    testCancel().myDebug.void

    // testExercise1()
    testExercise2()
    testExercise3()
  }
}