package part3concurrency

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

object BlockingIOs extends IOApp.Simple {

  val someSleeps = for {
    _ <- IO.sleep(1.seconds).myDebug // // SEMANTIC BLOCKING - no threads are actually blocked, CE assigns this thread to some other fiber
    _ <- IO.sleep(1.seconds).myDebug
  } yield ()

  //really blocking IOs
  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName} computed a blocking code")
    42
  } // will evaluate on a thread from ANOTHER thread pool specific for blocking calls

  // yielding
  val iosOnManyThreads = for {
    _ <- IO("first").myDebug
    _ <- IO.cede // a signal to yield control over the thread - equivalent to IO.shift
    _ <- IO("second").myDebug // the rest of this effect may run on another thread (not necessarily)
    _ <- IO.cede
    _ <- IO("third").myDebug
  } yield ()

  override def run: IO[Unit] = testThousandsEffectsSwitch().void

  /*
  Blocking calls & IO.sleep and yield control over the calling thread automatically.
  */

  def testThousandsEffectsSwitch() = {
    val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO.pure).reduce(_.myDebug >> IO.cede >> _.myDebug).evalOn(ec)
  }
}
