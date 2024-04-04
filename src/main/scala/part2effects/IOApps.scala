package part2effects

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.unsafe.implicits.global

import scala.io.StdIn

object IOApps {
  val program = for {
    line <- IO(StdIn.readLine())
    _ <- IO(println(s"You' hv just written: $line"))
  } yield ()
}

object TestApp {
  def main(args: Array[String]): Unit = {
    import IOApps._

    program.unsafeRunSync()
  }
}

object FirstCEApp extends IOApp {
  import IOApps._

  override def run(args: List[String]): IO[ExitCode] =
    program.map(_ => ExitCode.Success)
}

object MySimpleAppe extends IOApp.Simple {
  import IOApps._
  override def run: IO[Unit] = program
}