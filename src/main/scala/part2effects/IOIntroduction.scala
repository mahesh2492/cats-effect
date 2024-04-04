package part2effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {

  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) //arg should have side effects
  val aDelayedIO: IO[Int] = IO.delay {
    println("I'm producing an integer")
    42
  }

  val aDelayedIO_v2: IO[Int] = IO {
    println("I'm producing an integer")
    42
  }

  //map, flatMap
  val improvedMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  //mapN - combine IO effects as tuples
  import cats.syntax.apply._
  val combinedMeaningOfLife = (ourFirstIO, aDelayedIO).mapN(_ + _)

  def smallProgram_v2(): IO[String] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _)

  /**
   * exercise
   * 1. sequence two IOs and take the result of last one
   * 2. sequence two IOs and take the result of first one
   * 3. repeat an IO effect forever
   * 4. convert an IO to different type
   * 5. discard a value inside an IO, just return unit
   * 6. fix stack recursion
   */

  // 1
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = for {
    _ <- ioa
    b <- iob
  } yield b

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // "andThen"

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // "andThen" with by-name call

  // 2
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = for {
    a <- ioa
    _ <- iob
  } yield a

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  // 3
  def forever[A](io: IO[A]): IO[A] = io.flatMap(_ => forever(io))

  def forever_v2[A](io: IO[A]): IO[A] = io >> forever(io)

  def forever_v3[A](io: IO[A]): IO[A] = io *> forever(io)

  def forever_v4[A](io: IO[A]): IO[A] = io.foreverM

  // 4
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] = ioa.as(value)

  // 5
  def asUnit[A](ioa: IO[A]): IO[Unit] = ioa.map(_ => ())

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] = ioa.as(()) // discourage - don't use this

  def asUnit_v3[A](ioa: IO[A]): IO[Unit] = ioa.void // same and ecnouraged

  // 6
  def sum(n: Int): Int = {
    println(" number " + n)
    if(n <= 0) 0
    else n + sum(n - 1)
  }

  def sumIO(n: Int): IO[Int] =
    if(n <= 0) IO(0)
    else {
      for {
        lastNumber <- IO(n)
        prevSum <- sumIO(n - 1)
      } yield prevSum + lastNumber
    }

  // 7 - write a fibonacci IO that does not crash on recursion
  //hint use recursion
  def fibonacci(n: Int): IO[BigInt] =
    if(n < 2) IO(1)
    else for {
      last <- IO.defer(fibonacci(n - 1))
      prev <- IO.defer(fibonacci(n - 2))
    } yield last + prev

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global

    /*forever_v3(IO {
    println("forever!")
    Thread.sleep(100)
    }).unsafeRunSync()*/

    println(sumIO(20000).unsafeRunSync())
    //sum(20000)
    (1 to 100).foreach(i => println(fibonacci(i).unsafeRunSync()))

  }
}
