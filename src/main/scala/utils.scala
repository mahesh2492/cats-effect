import cats.effect.IO

package object tils {
  implicit class DebugWrapper[A](io: IO[A]) {
    def myDebug: IO[A] = for {
      a <- io
      t = Thread.currentThread().getName
      _ = println(s"[$t] $a")
    } yield a
  }
}
