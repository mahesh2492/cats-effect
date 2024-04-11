package part3concurrency

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object AsyncIOs extends IOApp.Simple {

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycles
  val threadPool = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit


  def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] compute the meaning of life on some other thread")
    42
  }

  def computeMeaningOfLifeEither(): Either[Throwable, Int] = Try(computeMeaningOfLife()).toEither

  def computeMolOnThreadPool(): Unit =
    threadPool.execute(() => computeMeaningOfLife())

  //lift computation to an IO
  val asyncMolIO: IO[Int] = IO.async_ { cb => //CE thread blocks (semantically) until this cb is invoked (by some other thread)
    threadPool.execute{() => // computations not managed by CE
      val result = computeMeaningOfLifeEither()
      cb(result) //CE thread is notified with the result
    }
  }

  /*
  Exercise
  */
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_{ cb =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        cb(result)
      }
    }

  val asyncMolIO_v2 = asyncToIO(computeMeaningOfLife)(ec)

  /*
  Exercise
  */
  lazy val molFuture = Future { computeMeaningOfLife() } (ec)
  def convertFutureToIO[A](future: => Future[A]): IO[A] = {
    IO.async_ { cb: Callback[A] =>
      future.onComplete{ tryResult =>
        val result = tryResult.toEither
        cb(result)
      }
    }
  }

  val asyncMolIO_v3 = convertFutureToIO(molFuture)
  val asyncMolIO_v4 = IO.fromFuture(IO(molFuture))

  /*
  Exercise: a never-ending IO ?
  */
  val neverEndingIO = IO.async_[Int](_ => ()) //no callback no finish
  val neverEndingIO_v2: IO[Int] = IO.never

  /*
  Full Async call
  */
  def demoAsyncCancellation() = {
    val asyncMeaningOfLife_v2 = IO.async { (cb: Callback[Int]) =>
      /*
      finalizer in case computation gets cancelled.
      finalizers are of type IO[Unit]
      not specifying finalizer => Option[IO[Unit]]
      creating option is an effect => IO[Option[IO[Unit]]]
      */
      //return IO[Option[IO[Unit]]]
      IO {
        threadPool.execute { () =>
          val result = computeMeaningOfLifeEither()
          cb(result)
        }
      }.as(Some(IO("Cancelled").myDebug.void))
    }

    for {
      fib <- asyncMeaningOfLife_v2.start
      _ <- IO.sleep(500.millis) >> IO("Cancelling").myDebug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] = demoAsyncCancellation() >> IO(threadPool.shutdown())
}
