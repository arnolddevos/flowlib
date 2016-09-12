package flowlib

import scala.language.higherKinds

import transducers.{
  Transducers,
  Views,
  Operators,
  AsyncEducers,
  Builders,
  ImmutableStateOperators,
  ContextIsMonad,
  Syntax }

import Process._
import ProcessUtil._
import Generators._

object Producers
  extends Transducers
  with Views
  with Operators
  with AsyncEducers
  with Builders
  with ImmutableStateOperators
  with ContextIsMonad
  with Syntax {

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

  type Producer[+A] = Generator[A] // back compatible name for Generator
  val Producer = Generator

  import Series._

  implicit def generatorIsEducible[A] = new Educible[Generator[A], A] {
    def educe[S](g: Generator[A], f: Reducer[A, S]): Process[S] = {
      def loop(g: Generator[A], s: f.State ): Process[S] = {
        if(f.isReduced(s)) stop(f.complete(s))
        else
          g >>= {
            _ match {
              case NonEmpty(a, g1) => f(s, a) >>= (loop(g1, _))
              case Empty => stop(f.complete(s))
            }
          }
      }
      loop(g, f.init)
    }
  }
}
