package flowlib
import scala.language.higherKinds

import Producer._

trait Producer[+T] {
  def step: Produced[T]
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

  type Produced[T] = Process[Option[(T, Producer[T])]] // same as Educed[T, Producer[T]]

  def producer[T](p: Produced[T]) = new Producer[T] { def step = p }

  implicit def producerEducer = new Educer[Producer] {
    def step[A]( p: Producer[A]) = p.step
  }
}
