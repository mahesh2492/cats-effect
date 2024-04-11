package part2effects

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

object IOParallelism extends IOApp.Simple {

  //IOs are usually sequential
  val aniIO = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread().getName}] Kamran")

  val composedIO = for {
    ani <- aniIO
    kamran <- kamranIO
  } yield s"$ani and $kamran love Rock the JVM"

  import cats.syntax.apply._
  val meaningOfLife: IO[Int] = IO.delay(42)
  val favLang: IO[String] = IO.delay("Scala")
  val goalInLife = (meaningOfLife, favLang).mapN((num, str) => s"my goal in life is $num and $str")

  import cats.Parallel
  import cats.effect.IO

  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.myDebug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.myDebug)

  import cats.effect.implicits._
  val goalInLifeParallel = (parIO1, parIO2).mapN((num, str) => s"my goal in life is $num and $str")

  //turn back into sequential
  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)
  import cats.syntax.parallel._

  //shorthand
  import cats.syntax.parallel._
  val goalInLife_v3: IO[String] = (meaningOfLife.myDebug, favLang.myDebug).parMapN((num, str) => s"my goal in life is $num and $str")


  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))

  //compose success + failure
  val parallelWithFailure: IO[String] = (meaningOfLife.myDebug, aFailure.myDebug).parMapN((num, str) => s"my goal in life is $num and $str")
  //compose failure + failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("second failure"))
  val twoFailures: IO[String] = (aFailure.myDebug, anotherFailure.myDebug).parMapN(_ + _)
  // the first effect to fail gives the failure of the result
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFailure.myDebug, anotherFailure.myDebug).parMapN(_ + _)


  override def run: IO[Unit] = {
    //composedIO.map(println)
    //goalInLife_v2.map(println)
    // goalInLife_v3.map(println)
    //parallelWithFailure.myDebug.void
    //twoFailures.myDebug.void
    twoFailuresDelayed.myDebug.void
  }
}