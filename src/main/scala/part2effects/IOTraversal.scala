package part2effects

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.Future

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String) = Future {
    Thread.sleep(scala.util.Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad = List("I quite like CE", "Scala is great.", "Looking forward to see some awesome stuff.")

  def clunkyFuture(): Unit = {
    val future = workLoad.map(heavyComputation)
    future.foreach(_.foreach(println))
  }

  import cats.Traverse
  import cats.instances.list._
  val listTraverse = Traverse[List]

  def traverseFuture():Unit = {
    val singleFuture = listTraverse.traverse(workLoad)(heavyComputation)
    singleFuture.foreach(println)
  }

  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(scala.util.Random.nextInt(1000))
    string.split(" ").length
  }

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO).myDebug

  //parallel traversal
  import cats.syntax.parallel._
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /*
     Exercise
   */
  //hint use traverse api
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listTraverse.traverse(listOfIOs)(identity)

  def sequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] = Traverse[F].traverse(wrapperOfIOs)(identity)

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listOfIOs.parTraverse(identity)

  def parSequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] = wrapperOfIOs.parTraverse(identity)

  //existing sequence API
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)

  //parallel sequencing
  val parallelSingleIO_v2 = parSequence(ios) //from the exercise
  val parallelSingleIO_v3 = ios.parSequence //extension method from the Parallel Syntax Package

  override def run: IO[Unit] = {
    parallelSingleIO_v3.map(_.sum).myDebug.void
  }

}