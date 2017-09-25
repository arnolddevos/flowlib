package flowlib

import scala.language.higherKinds

import transducers.lifted.API
import Process._
import ProcessUtil._

object Producers extends API {

  type Context[+S] = Process[S]
  def inContext[S](s: S) = stop(s)
  def lazyContext[S](s: => S) = lazily(s)
  def mapContext[S, T](p: Process[S])( f: S => T ) = p map f
  def bindContext[S, T](p: Process[S])(f: S => Process[T]): Process[T] = p flatMap f

  def stream[G, A](g: G)(implicit e: Educible[G, A]): Sink[Option[A]] => Process[Unit] = {
    output =>
      val f = reducer(())((_, a: A) => output(Some(a)))
      reduce(g, f) >> output(None)
  }

  def emit[G, A](g: G)(implicit e: Educible[G, A]): Sink[A] => Process[Unit] = {
    output =>
      val f = reducer(())((_, a: A) => output(a))
      reduce(g, f)
  }

  def unstream[A, S]( f: Reducer[A, S]): Source[Option[A]] => Process[S] = {
    input =>
      def loop(s: f.State): Process[S] = {
        if(f.isReduced(s)) f.complete(s)
        else
          input >>= {
            case Some(a) => f(s, a) >>= loop
            case None => f.complete(s)
          }
      }
      f.init >>= loop
  }

  def absorb[A, S]( f: Reducer[A, S]): Source[A] => Process[S] = {
    input =>
      def loop(s: f.State): Process[S] = {
        if(f.isReduced(s)) f.complete(s)
        else input >>= (f(s, _)) >>= loop
      }
      f.init >>= loop
  }

  implicit def streamIsEducible[A] = new Educible[Source[Option[A]], A] {
    def educe[S](s: Source[Option[A]], f: Reducer[A, S]) = unstream(f)(s)
  }
}
