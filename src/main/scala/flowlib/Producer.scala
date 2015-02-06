package flowlib
import scala.language.higherKinds

import Producer._

trait Producer[+A] {
  def step: Produced[A]
}

object Producer extends Transducers {
  import Process._

  type Context[+S] = Process[S]
  def inContext[S](s: S) = stop(s)
  def mapContext[S, T](p: Process[S])( f: S => T ) = p map f
  
  implicit def isEducible[R[_]](implicit e: Educer[R]) = new Educible[R] {
    def educe[A, S](ra: R[A], f: Reducer[A, S]): Process[S] = {
      def loop(ra: R[A], sf: f.State ): Process[S] = {
        if(f.isReduced(sf)) 
          stop(f.complete(sf))
        else  
          e.step(ra) >>= {
            case Some((a, ra1)) =>
              f(sf, a) >>= {
                sf1 =>
                  loop(ra1, sf1)
              }
            case None => stop(f.complete(sf))
          }
      }
      loop(ra, f.init)
    }
  }

  type Produced[+A] = Process[Option[(A, Producer[A])]] // same as Educed[A, Producer[A]]

  def producer[A](p: Produced[A]) = new Producer[A] { def step = p }

  def emit[A](value: A)(resume: Producer[A]): Produced[A] = stop(Some((value, resume)))

  val emitEnd: Produced[Nothing] = stop(None)

  val emptyProducer: Producer[Nothing] = producer(emitEnd)

  implicit def producerEducer = new Educer[Producer] {
    def step[A]( p: Producer[A]) = p.step
  }
}
