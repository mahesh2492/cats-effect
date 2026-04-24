package part3concurrency

import cats.effect.{IO, IOApp}
import utils.DebugWrapper
import scala.concurrent.duration._

object CancelingIOs extends IOApp.Simple {

  val specialPaymentSystem = (
    IO("Payment running, don't cancel me...").myDebug >>
      IO.sleep(1.second) >>
      IO("Payment completed.").myDebug
    ).onCancel(IO("Mega cancel of Doom!").myDebug.void)

  val cancellationOfDooms = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.milli) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem) // "masking"
  val atomicPayment_v2 = specialPaymentSystem.uncancelable // "masking"
  val noCancellation = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.milli) >> IO("Attempting cancellation..").myDebug >> fib.cancel
    _ <- fib.join
  } yield ()

  /*
   The uncancelable API is more complex and more general.
   It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
   The Poll object can be used to mark sections within the returned effect which CAN BE CANCELED.
  */

  /*
   Example: authentication service. Has two parts:
   - input password, can be cancelled, because otherwise we might block indefinitely on user input
   - verify password, CANNOT be cancelled once it's started
  */

  val inputPassword = IO("Input Password:").myDebug >> IO("typing password").myDebug >> IO.sleep(3.seconds) >> IO("RockTheJVM1!")
  val verifyPassword = (pw: String) => IO("verifying...").myDebug >> IO.sleep(2.seconds) >> IO(pw == "RockTheJVM1!")

  val authFlow = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out. Try again later.").myDebug.void) // this is cancellable
      verified <- verifyPassword(pw) // this is not cancellable
      _ <- if(verified) IO("Authentication successful.").myDebug
            else IO("Authentication failed").myDebug
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(1.second) >> IO("Authentication timeout, attempting cancel...").myDebug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  /*
      Uncancelable calls are MASKS which suppress cancellation.
      Poll calls are "gaps opened" in the uncancelable region.
   */

  /*
      Exercise: what do you think the following effects will do ?
      1. Anticipate
      2. Run to see if you're correct.
      3. Prove your theory
   */
  //1
  val cancelBeforeMol = IO.canceled >> IO(42).myDebug
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).myDebug)
  // uncancelable will eliminate ALL cancel points

  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(1.seconds) >> IO("Authenticated timeout, attempting cancel...").myDebug >> authFib.cancel
    _ <- authFib.join
  } yield ()
/*
    Lesson: Uncancelable calls are masks which suppress all existing cancelable gaps (including from a previous uncancelable).
 */

  def threeStepProgram() = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("canelable").myDebug) >> IO.sleep(1.seconds) >> IO("cancelable end").myDebug >>
        IO("uncancelable").myDebug >> IO.sleep(1.seconds) >> IO("uncancelable end").myDebug >>
        poll(IO("second cancelable").myDebug >> IO.sleep(1.second) >> IO("second cancelable end").myDebug)
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(1500.millis) >> IO("Canceling").myDebug >> fib.cancel
      _ <- fib.join
    } yield ()
  }
  /*
      Lesson: Uncancelable regions ignore cancellation signals, but that doesn't mean the next CANCELABLE region won't take them.
   */

  override def run: IO[Unit] = threeStepProgram().void
}

