package flowlib

trait Acceptor[-T] {
  def accept(t: T): Unit
}

object Acceptor {
  implicit class AcceptorOps[T](inner: Acceptor[T]) {
    def contramap[S]( f: S => T) = new Acceptor[S] {
      def accept(s: S) = inner.accept(f(s))
    }
  }

  import scala.util.Try
  import scala.concurrent.Promise
  implicit class PromiseAcceptor[T]( pr: Promise[T] ) extends Acceptor[Try[T]] {
    def accept(t: Try[T]): Unit = pr complete t
  }
}
