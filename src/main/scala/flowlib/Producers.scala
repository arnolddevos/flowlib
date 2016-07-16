package flowlib

import scala.language.higherKinds

import transducers.{Transducers, Views, Operators, AsyncEducers, ContextIsMonad}

import Process._
import ProcessUtil._
import Generators._

object Producers extends Transducers with Views with Operators with AsyncEducers with ContextIsMonad {

  type Context[+S] = Process[S]
  def inContext[S](s: S) = stop(s)
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
        if(f.isReduced(s)) stop(f.complete(s))
        else
          input >>= {
            case Some(a) => f(s, a) >>= loop
            case None => stop(f.complete(s))
          }
      }
      loop(f.init)
  }

  def absorb[A, S]( f: Reducer[A, S]): Source[A] => Process[S] = {
    input =>
      def loop(s: f.State): Process[S] = {
        if(f.isReduced(s)) stop(f.complete(s))
        else
          input >>= (f(s, _)) >>= loop
      }
      loop(f.init)
  }


  import Series._

  type Producer[+A] = Process[Series[A]]

  object Producer {
    val empty: Producer[Nothing] = stop(Empty)
    def apply() = empty
    def apply[A](a: A, as: Producer[A]): Producer[A] = stop(NonEmpty(a, as))
  }

  implicit def producerIsEducible[A] = new Educible[Producer[A], A] {
    def educe[S](as: Producer[A], f: Reducer[A, S]): Process[S] = {
      def loop(as: Producer[A], sf: f.State ): Process[S] = {
        if(f.isReduced(sf))
          stop(f.complete(sf))
        else
          as >>= {
            case NonEmpty(a, as1) =>
              f(sf, a) >>= {
                sf1 =>
                  loop(as1, sf1)
              }
            case Empty => stop(f.complete(sf))
          }
      }
      loop(as, f.init)
    }
  }
}
