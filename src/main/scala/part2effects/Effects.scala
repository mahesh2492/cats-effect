package part2effects

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Effects {

  // pure functional programming
  //substitution
  def combine(a: Int, b: Int): Int = a + b
  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  //referential transparency = can replace an expression with its value
  // as many times as we want without changing behaviour

  //example: print to the console
  val printSomething: Unit = println("Cats Effect")
  val printSomething_v2: Unit = ()

  //example: changing a variable
  var anInt = 0
  val changingVar: Unit = (anInt += 1)
  val changingVar_v2: Unit = () // not the same

  //side effects are inevitable for useful programs.

  /*
  Effect types
  Properties:
  - type signature describes the kind of calculations that will be performed
  - type signature describes the VALUE that will be calculated
  - when side effects are needed, effect construction is separate from execution
  */

  /*
  example: Option is an effect type
  - describes a possibly absent values
  - computes a value of type A, if it exists
  - side effects are not needed
  */
  val anOption: Option[Int] = Option(42)

  /*
  example: Future is not an effect type
  - describes an asynchronous computation
  - computes a value of type A, if it's successful
  - side effect is required (allocating/scheduling a thread), execution is not separate from execution
  */
  val aFuture = Future(42)

  /*
  example: MyIO data type is an effect type
  - describes any computation that might produce side effects
  - calculates a value of type A, if it's successful
  - side effects are required for the evaluation of () => A
  - YES, the creation of MyIO does not produce the side effects on construction
  */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I am writing something...")
    42
  })

  /**
   * Exercise
   * 1. An IO which returns the current time of the system
   * 2. An IO which returns the duration of the computation (hint: use ex 1)
   * 3. An IO which reads a line (a string) from the std input
   * 4. An IO which prints something to the console
   */

  val clock: MyIO[Long] = MyIO(() => {
    System.currentTimeMillis()
  })

  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    start <- clock
    _ <- computation
    end <- clock
  } yield end - start

  /*
  currentTime.flatMap(start => computation.flatMap(_ => currentTime.map(end => end - start)))
  currentTime.map(end => end - start)) = MyIO(() => currentTime.System.currentTimeMillis() - start)
  */

  def testTimeIO: Unit = {
    val test = measure(MyIO(() => Thread.sleep(1000)))
    println(test.unsafeRun())
  }

  val read: MyIO[String] = MyIO(() => scala.io.StdIn.readLine())

  def putStrLn(input: String): MyIO[Unit] = MyIO(() => println(input))

  def testConsole(): Unit = {
    val program = for {
      line1 <- read
      line2 <- read
      _ <- putStrLn(line1 + line2)
    } yield ()

    program.unsafeRun
  }

  def main(args: Array[String]): Unit = {
    testTimeIO

    testConsole() //Todo fix this
  }
}