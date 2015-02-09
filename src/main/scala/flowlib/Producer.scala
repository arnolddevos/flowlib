package flowlib
import scala.language.higherKinds
 
import Process._
import ProcessUtil._
import Producer._

trait Producer[+A] { parent =>
  def step: Produced[A]
  def +:[B >: A](b: B): Produced[B] = stop(Some(b, parent))
}

object Producer extends Transducers {

  type Context[+S] = Process[S]
  def inContext[S](s: S) = stop(s)
  def mapContext[S, T](p: Process[S])( f: S => T ) = p map f

  implicit def producerEducer = new Educer[Producer] {
    def step[A]( p: Producer[A]) = p.step
  }
  
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

  val nothingProduced: Produced[Nothing] = stop(None)

  def producer[A](p: Produced[A]) = new Producer[A] { def step = p }

  val emptyProducer: Producer[Nothing] = producer(nothingProduced)

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
}
