package part4coordination

import cats.effect.{Deferred, Fiber, IO, IOApp, Outcome, Ref}
import cats.implicits.toTraverseOps
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt

object Defers extends IOApp.Simple {

  //deferred is a primitive for waiting for an effect, while some other effect complete with a value

  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val aDeferred_v2: IO[Deferred[IO, Int]] = IO.deferred[Int]

  //get blocks the calling fiber (semantically) until some other fiber completes the Deferred with a value
  val reader: IO[Int] = aDeferred.flatMap { signal =>
    signal.get //blocks the fiber
  }

  val writer = aDeferred.flatMap { signal =>
    signal.complete(42)
  }

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = {
      for {
        _ <- IO("[Consumer] Waiting for the result..").myDebug
        meaningOfLife <- signal.get
        _ <- IO(s"[Consumer] got the result: $meaningOfLife").myDebug
      } yield ()
    }

    def producer(signal: Deferred[IO, Int]) = {
      for {
        _ <- IO("[producer] crunching numbers... ").myDebug
        _ <- IO.sleep(1.seconds)
        _ <- IO("[producer] complete 42").myDebug
        meaningOfLife <- IO(42)
        _ <- signal.complete(meaningOfLife)
      } yield ()

    }

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibProducer.join
      _ <- fibConsumer.join
    } yield ()
  }

  //simulate downloading the content
  val fileParts = List("I", "love S", "cala", " with Cat", "s Effect!<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]) = {
      fileParts.map { part =>
        IO(s"Got part $part").myDebug >> IO.sleep(1.seconds) >> contentRef.update(current => current + part)
      }
        .sequence
        .void
    }

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <- if(file.endsWith("<EOF>")) IO("File download complete").myDebug
      else IO("downloading...").myDebug >> IO.sleep(1.second) >> notifyFileComplete(contentRef)
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fileDownloader <- downloadFile(contentRef).start
      notifier <- notifyFileComplete(contentRef).start
      _ <- fileDownloader.join
      _ <- notifier.join
    } yield ()
  }

  def fileNotifierWithDeferred(): IO[Unit] = {
    def downloadFile(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]) = {
      for {
        _ <- IO(s"[Downloader] got part $part").myDebug
        _ <- IO.sleep(1.seconds)
        latestContent <- contentRef.updateAndGet(current => current + part)
        _ <- if(latestContent.contains("<EOF>")) signal.complete(latestContent) else IO.unit
      } yield ()
    }

    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[notifier] downloading..").myDebug
      _ <- signal.get // blocks until the signal is completed
      _ <- IO("[notifier] File download complete").myDebug
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      signal <- Deferred[IO, String]
      notifier <- notifyFileComplete(signal).start
      fileDownloader <- fileParts.map(part => downloadFile(part, contentRef, signal)).sequence.start
      _ <- fileDownloader.join
      _ <- notifier.join
    } yield ()
  }

  /**
   * Exercises:
   * - (medium) write a small alarm notification with two simultaneous IOs
   * - one that increments a counter every second (a clock)
   * - one that waits for the counter to become 10, then prints a message "time's up!"
   *
   * - (mega hard) implement racePair with Deferred.
   * - use a Deferred which can hold an Either[outcome for ioa, outcome for iob]
   * - start two fibers, one for each IO
   * - on completion (with any status), each IO needs to complete that Deferred
   * (hint: use a finalizer from the Resources lesson)
   * (hint2: use a guarantee call to make sure the fibers complete the Deferred)
   * - what do you do in case of cancellation (the hardest part)?
   */
  // 1

  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]), // (winner result, loser fiber)
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B]) // (loser fiber, winner result)
  ]

  override def run: IO[Unit] = fileNotifierWithDeferred()
}