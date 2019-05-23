import scalaz._, Scalaz._
import simulacrum._
import scala.concurrent.Future

/**
  * We would like to avoid specifying
  * equal traits for different possible
  * executions.
  */
object Terminal {
  trait TerminalSyncDuplicated {
    def read(): String
    def write(t: String): Unit
  }

  trait TerminalAsyncDuplicated {
    def read(): Future[String]
    def write(t: String): Future[Unit]
  }

  /**
    * Think of C as a ''Context''
    */
  trait Terminal[C[_]] {
    def read: C[String]
    def write(t: String): C[Unit]
  }

  type Now[X] = X

  object TerminalSync extends Terminal[Now] {
    def read: Now[String] = ???
    def write(t: String): Now[Unit] = ???
  }

  object TerminalAsync extends Terminal[Future] {
    def read: Future[String] = ???
    def write(t: String): Future[Unit] = ???
  }

  trait ExecutionNaive[C[_]] {
    def chain[A, B](c: C[A])(f: A => C[B]): C[B]
    def create[B](b: B): C[B]
  }

  /**
    * The [[fp.Terminal.ExecutionNaive]] allows to re-use
    * echo implementation for both Sync and Async
    * versions.
    */
  def echo[C[_]](t: Terminal[C], e: ExecutionNaive[C]): C[String] =
    e.chain(t.read) { in: String =>
      e.chain(t.write(in)) { _: Unit =>
        e.create(in)
      }
    }
}
