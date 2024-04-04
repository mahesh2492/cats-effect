package part2effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object IOErrorHandling {

  //create failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A Failure"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException(("a proper fail")))

  //handle exception
  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("I'm still here"))
  }

  //turn into a Either
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt
  //redeem: transform the failure and the success in one go
  val resultAsString = aFailure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: $value")
  //redeemWith
  val resultAsEffect = aFailure.redeemWith(ex => IO(println(s"FAIL: $ex")), value => IO(println(s"SUCCESS: $value")))

  /*
  Exercise
  1. construct potentially failed IOs from standard data types (Option, Try, Either)
  */

  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = {
    option match {
      case Some(value) => IO.pure(value)
      case None => IO.raiseError(ifEmpty)
    }
  }

  def option2IO_V2[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    IO.fromOption(option)(ifEmpty)

  def try2IO[A](aTry: Try[A]): IO[A] = {
    aTry match {
      case Success(value) => IO.pure(value)
      case Failure(exception) => IO.raiseError(exception)
    }
  }

  def try2IO_V2[A](aTry: Try[A]): IO[A] =
    IO.fromTry(aTry)

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] = {
    anEither match {
      case Right(value) => IO.pure(value)
      case Left(exception) => IO.raiseError(exception)
    }
  }

  def either2IO_V2[A](anEither: Left[Throwable, A]): IO[A] =
    IO.fromEither(anEither)

  //handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem(handler, identity)


  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.redeemWith(handler, IO.pure)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    dealWithIt.unsafeRunSync()
    println(resultAsString.unsafeRunSync())

    resultAsEffect.unsafeRunSync()
  }
}