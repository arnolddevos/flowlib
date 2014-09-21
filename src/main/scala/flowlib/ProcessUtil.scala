package flowlib

object ProcessUtil {
  import Process._
  import scala.collection.immutable.Queue

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

  def cat[A]: Process[A] => (A => Process[Unit]) => Process[Nothing] = {
    source => sink =>
      def loop: Process[Nothing] = (source >>= sink) >> loop
      loop
  }

  def fanout[T]( members: List[T => Process[Unit]]): T => Process[Unit] = { t =>
    def loop(members: List[T => Process[Unit]]): Process[Unit] = members match {
      case member :: rest => member(t) >> loop(rest)
      case Nil => stop(())
    }
    loop(members)
  }
}
