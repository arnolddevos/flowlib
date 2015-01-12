package flowlib

object ProcessUtil {
  import Process._
  import Folder._

  def background[U](p0: Process[U]): Process[Process[U]] = {
    val g = Gate.observable[U]()
    (p0 >>= { u => g signal Some(u); stop(u) }) & stop(Waiting(g.take))
  }

  def join[A, B]: Process[A] => Process[B] => Process[(A,B)] = {
    pa => pb =>
      background(pa) >>= { pa1 =>
        pb >>= {  b =>
          pa1 >>= { a =>
            stop((a, b))
          }
        }
      }
  }

  def forever(p: Process[Any]): Process[Nothing] =
    p >> forever(p)

  def cat[A]: Process[A] => (A => Process[Unit]) => Process[Nothing] =
    source => sink => forever(source >>= sink) 

  def valve[A]: Process[Any] => Process[A] => (A => Process[Unit]) => Process[Nothing] =
    control => source => sink => forever(control >> (source >>= sink))

  def sendTo[A](g: Gate[A, Any])(a: A): Process[Unit] = 
    waitDone(g offer a)

  def takeFrom[A](g: Gate[Any, A]): Process[A] =
    waitFor(g.take) 

  def fanout[T]( sinks: List[T => Process[Unit]]): T => Process[Unit] =
    t => sequence(sinks).iterate(sink => sink(t))
}
