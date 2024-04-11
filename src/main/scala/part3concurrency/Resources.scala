package part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.{IO, IOApp, Resource}
import utils.DebugWrapper

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object Resources extends IOApp.Simple {

  class Connection(url: String) {
    def open() = IO(s"opening connections to $url").myDebug
    def close() = IO(s"closing connections to $url").myDebug
  }

  val asyncFetchURl = for {
    fib <- (new Connection("rockthejvm.com").open() >> IO.sleep((Int.MaxValue).seconds)).start
    _ <- IO.sleep(1.seconds) >> fib.cancel
  } yield {}
  //problem: leaking resources

  val correctAsyncFetch = for {
    conn <- IO(new Connection("rockthejvm.com"))
    fib <- (conn.open() >> IO.sleep((Int.MaxValue).seconds)).onCancel(conn.close().void).start
    _ <- IO.sleep(1.seconds) >> fib.cancel
  } yield {}

  /*
  bracket pattern - someIO.bracket(useResourceCb)(releaseResourceCb)
  bracket is equivalent to try-catches (pure fp)
  */
  val bracketFetchUrl = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open() *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close().void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.seconds) *> fib.cancel
  } yield ()

  /*
  Exercise - read the file with the bracket pattern
  - open a scanner
  - read the file line by line, every 100 millis
  - close the scanner
  - if cancelled/throws error, close scanner
  */
  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readFileLineByLine(scanner: Scanner): IO[Unit] =
    if(scanner.hasNext) IO(scanner.nextLine()).myDebug >> IO.sleep(100.millis) >> readFileLineByLine(scanner)
    else IO.unit

  def bracketReadFile(path: String) =
    IO(s"Opening file at $path") >>
      openFileScanner(path).bracket { scanner =>
        readFileLineByLine(scanner)
      } { scanner =>
        IO(s"Closing the file at $path").myDebug >> IO(scanner.close())
      }

  /*
  Resources
  */
  def connectionFromConfig(path: String): IO[Unit] =
    openFileScanner(path).bracket{ scanner =>
      //acquire a connection based on the file
      IO(new Connection(scanner.nextLine())).bracket { conn =>
        conn.open().myDebug >> IO.never
      }(conn => conn.close().myDebug.void)
    } (scanner => IO("closing file").myDebug >> IO(scanner.close()))
  //nesting resources are tedious

  val connectionResource = Resource.make(IO(new Connection("rockthejvm.com")))(conn => conn.close().void)
  // .. at a later part of your code - it can be used

  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
    _ <- IO.sleep(1.seconds) >> fib.cancel
  } yield ()

  //resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResources: String => IO[String] = string => IO(s"using the resources $string").myDebug
  val releaseResources: String => IO[Unit] = string => IO(s"releasing the resources $string").myDebug.void

  val usingResourceWithBracket = simpleResource.bracket(usingResources)(releaseResources)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResources).use(usingResources)

  /**
   * Refactor bracket exercise using Resources
   *
   */
  def getResourceFromFile(path: String) = Resource.make(openFileScanner(path)) { scanner =>
    IO(s"Closing the file at $path").myDebug >> IO(scanner.close())
  }

  def resourceReadFile(path: String): IO[Unit] =
    IO(s"Opening file at $path") >>
      getResourceFromFile(path).use { scanner =>
        readFileLineByLine(scanner)
      }

  def cancelReadFile(path: String) = for {
    fib <- resourceReadFile(path).start
    _ <- IO.sleep(2.seconds) >> fib.cancel
  } yield ()

  //nested resources
  def connFromConfResources(path: String) =
    Resource.make(IO("opening file").myDebug >> openFileScanner(path))(scanner => IO("closing file").myDebug >> IO(scanner.close()))
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void))

  //equivalent
  def connFromConfResourcesClean(path: String) = for {
    scanner <- Resource.make(IO("opening file").myDebug >> openFileScanner(path)) (scanner => IO("closing file").myDebug >> IO(scanner.close()))
    conn <- Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void)
  } yield conn

  val openConnections =
    connFromConfResources("C:\\Users\\mahesh.chand\\IdeaProjects\\Projects\\cats-effect\\src\\main\\resources\\connection.txt")
      .use(conn => conn.open() >> IO.never)

  val closedConnection = for {
    fib <- openConnections.start
    _ <- IO.sleep(1.seconds) >> IO("cancelling!").myDebug >> fib.cancel
  } yield ()

  //finalizer to regular IOs
  val ioWithFinalizer = IO("some resources").myDebug.guarantee(IO("freeing resources").myDebug.void)
  val ioWithFinalizer_v2 = IO("some resources").myDebug.guaranteeCase {
    case Outcome.Succeeded(fa) => fa.flatMap(result => IO("freeing resources").myDebug.void)
    case Outcome.Errored(e) => IO("nothing to release").myDebug.void
    case Outcome.Canceled() => IO("resources got cancelled, releasing what's left").myDebug.void
  }

  override def run: IO[Unit] = {
    //bracketReadFile("C:\\Users\\mahesh.chand\\IdeaProjects\\Projects\\cats-effect\\src\\main\\scala\\part3fiber\\Resources.scala")
    //resourceFetchUrl.void
    //resourceReadFile("C:\\Users\\mahesh.chand\\IdeaProjects\\Projects\\cats-effect\\src\\main\\scala\\part3fiber\\Resources.scala").void
    // cancelReadFile("C:\\Users\\mahesh.chand\\IdeaProjects\\Projects\\cats-effect\\src\\main\\scala\\part3fiber\\Resources.scala").void
    // closedConnection
    ioWithFinalizer.void
    ioWithFinalizer_v2.void
  }
}