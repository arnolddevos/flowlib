package flowlib

trait Responder[+T] {
  def respond( k: T => Unit ): Unit
}

object Responder {
  def constant[T](t: T) = new Responder[T] {
    def respond( k: T => Unit ): Unit = k(t)
  }

  def empty[T] = new Responder[T] {
    def respond( k: T => Unit ): Unit = ()
  }

  implicit class ResponderOps[T](inner: Responder[T]) {
    def map[S]( f: T => S ) = new Responder[S] {
      def respond( k: S => Unit ) = inner respond  (f andThen k)
    }
  }

  import scala.util.Try
  import scala.concurrent.{Future, ExecutionContext}
  implicit class FutureResponder[T]( ft: Future[T])(implicit ec: ExecutionContext) extends Responder[Try[T]] {
    def respond( k: Try[T] => Unit ) = ft onComplete k
  }

  import scala.{Responder => LegacyResponder}
  implicit class Adapter[T]( lr: LegacyResponder[T]) extends Responder[T] {
    def respond( k: T => Unit ) = lr respond k
  }
}
