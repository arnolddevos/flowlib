package flowlib

import scala.language.higherKinds

import transducers.{ Transducers, Views, Operators, Builders, ContextIsMonad, Syntax }
import transducers.lifted.{Educers, StatefulOperators}

import Process._
import ProcessUtil._
import Generators._

object Producers
  extends Transducers
  with Views
  with Operators
  with Builders
  with ContextIsMonad
  with Syntax
  with Educers
  with StatefulOperators
{
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

  type Producer[+A] = Generator[A] // back compatible name for Generator
  val Producer = Generator

  import Series._

  implicit def generatorIsEducible[A] = new Educible[Generator[A], A] {
    def educe[S](g: Generator[A], f: Reducer[A, S]): Process[S] = {
      def loop(g: Generator[A], s: f.State ): Process[S] = {
        if(f.isReduced(s)) f.complete(s)
        else
          g >>= {
            _ match {
              case NonEmpty(a, g1) => f(s, a) >>= (loop(g1, _))
              case Empty => f.complete(s)
            }
          }
      }
      f.init >>= (loop(g, _))
    }
  }
}
