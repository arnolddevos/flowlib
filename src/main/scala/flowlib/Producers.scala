package flowlib

import scala.language.higherKinds
 
import transducers.{Transducers, Views, Operators, AsyncEducers}

import Process._
import ProcessUtil._

sealed trait Series[+A]

object Series {
  case class NonEmpty[+A](head: A, tail: Process[Series[A]]) extends Series[A]
  case object Empty extends Series[Nothing]
}


object Producers extends Transducers with Views with Operators with AsyncEducers {

  type Context[+S] = Process[S]
  def inContext[S](s: S) = stop(s)
  def mapContext[S, T](p: Process[S])( f: S => T ) = p map f
  def bindContext[S, T](p: Process[S])(f: S => Process[T]): Process[T] = p flatMap f

  def stream[R[_]: Educible, A](ra: R[A]): Sink[Option[A]] => Process[Unit] = {
    output =>
      val f = reducer(())((_, a: A) => output(Some(a)))
      educe(ra, f) >> output(None)
  }

  def emit[R[_]: Educible, A](ra: R[A]): Sink[A] => Process[Unit] = {
    output =>
      val f = reducer(())((_, a: A) => output(a))
      educe(ra, f)
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

  implicit val producerIsEducible = new Educible[Producer] {
    def educe[A, S](as: Producer[A], f: Reducer[A, S]): Process[S] = {
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
