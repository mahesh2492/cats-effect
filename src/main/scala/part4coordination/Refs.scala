package part4coordination

import cats.effect.{IO, IOApp, Ref}
import cats.implicits.{catsSyntaxParallelSequence1, catsSyntaxTuple2Parallel}
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt

object Refs extends IOApp.Simple {

  //ref = purely functional atomic structure
  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(42)

  val increasedMol = atomicMol.flatMap { ref =>
    ref.set(43) //thread safe
  }

  //obtain a value
  val mol = atomicMol.flatMap { ref =>
    ref.get // thread safe
  }

  val gsMol = atomicMol.flatMap { ref =>
    ref.getAndSet(43) //thread safe
  } //gets the old value and sets the new one

  //updating with a function
  val fMol = atomicMol.flatMap { ref =>
    ref.update(value => value * 10)
  }

  val updatedMol = atomicMol.flatMap { ref =>
    ref.updateAndGet(value => value * 10) //get the new value
    //can also use getAndUpdate to get the OLD value
  }

  //modifying with a function returning a different type
  val modifyMol = atomicMol.flatMap { ref =>
    ref.modify(value => (value * 10, s"my current value is $value"))
  }

  //why: concurrent + thread-safe reads/writes over shared values, in a purely functional way

  def demoCurrentWorkImpure() = {
    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount'").myDebug
        newCount <- IO(count + wordCount)
        _ <- IO(s"New Total: $newCount").myDebug
        _ <- IO(count += wordCount)
      } yield ()
    }

    List("I love cats effects", "This ref things is useless", "Mahesh writes a lot of code.")
      .map(task)
      .parSequence
      .void

    /*
    Drawbacks:
    - hard to read/debug
    - mix pure/impure code
    - Not thread safe
    */
  }

  def demoCurrentWorkPure(): IO[Unit] = {

    def task(workload: String, totalCount: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount'").myDebug
        newCount <- totalCount.updateAndGet(currentCount => currentCount + wordCount)
        _ <- IO(s"New Total: $newCount").myDebug
      } yield ()
    }

    for {
      initialCount <- Ref[IO].of(0)
      _ <- List("I love cats effects", "This ref things is useless", "Mahesh writes a lot of code.")
        .map(string => task(string, initialCount))
        .parSequence
        .void
    } yield ()

  }

  /*
  * Exercise
  */
  def tickingClockImpure(): IO[Unit] = {
    var ticks = 0L

    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.seconds)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- IO(ticks += 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").myDebug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def tickingClockPure(): IO[Unit] = {

    def tickingClock(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(1.seconds)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      t <- ticks.get
      _ <- IO(s"TICKS: $t").myDebug
      _ <- printTicks(ticks)
    } yield ()

    for {
      initial <- Ref[IO].of(0)
      _ <- (tickingClock(initial), printTicks(initial)).parTupled
    } yield ()
  }

  override def run: IO[Unit] = tickingClockPure()
}