import scalaz._, Scalaz._
import simulacrum._
import scala.concurrent.Future

/**
  * We would like to avoid specifying
  * equal traits for different possible
  * executions.
  *
  * If we write methods that operate on
  * monadic types, then we can write
  * sequential code that abstracts over
  * it's execution context.
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
    * Think of C as a ''Context''. But we know nothing about C[_]
    * and we cannot do anything on with C[String].
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

  /**
    * Execution environment that lets us call a
    * method, returning C[T] and do something
    * with the T, including calling another
    * method on [[fp.Terminal.Terminal]].
    *
    * This is same as [[scalaz.Monad]]. We say
    * that `C` is ''monadic'' when there is an
    * implicit Monad[C] available.
    */
  trait Execution[C[_]] {
    def chain[A, B](c: C[A])(f: A => C[B]): C[B]
    def create[B](b: B): C[B]
  }

  /**
    * The [[fp.Terminal.Execution]] allows to re-use
    * echo implementation for both Sync and Async
    * versions.
    */
  def echoNaive[C[_]](t: Terminal[C], e: Execution[C]): C[String] =
    e.chain(t.read) { in: String =>
      e.chain(t.write(in)) { _: Unit =>
        e.create(in)
      }
    }

  /**
    * Add usefull combinators to the `C[_]` using implicits.
    */
  object Execution {
    implicit class Ops[A, C[_]](c: C[A]) {
      def flatMap[B](f: A => C[B])(implicit e: Execution[C]): C[B] =
        e.chain(c)(f)

      def map[B](f: A => B)(implicit e: Execution[C]): C[B] =
        e.chain(c)(f andThen e.create)
    }
  }

  import Execution.Ops
  def echoImproved[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
    t.read.flatMap { in: String =>
      t.write(in).map { _: Unit =>
        in
      }
    }

  def echo[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
    for {
      in <- t.read
      _ <- t.write(in)
    } yield in
}
