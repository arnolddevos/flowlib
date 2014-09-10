package flowlib

object ProcessUtil {
  import Process._
  import scala.collection.immutable.Queue

  def background[U](p0: Process[U]): Process[Process[U]] = {
    val g = Gate.observable[U]()
    (p0 >>= { u => g signal Some(u); stop(u) }) & stop(Waiting(g.take))
  }

  def join[A, B](pa: Process[A], pb: Process[B]): Process[(A,B)] = {
    background(pa) >>= { pa1 =>
      pb >>= {  b =>
        pa1 >>= { a =>
          stop((a, b))
        }
      }
    }
  }

  def fanout[T]( members: List[T => Process[Unit]]): T => Process[Unit] = { t =>
    def loop(members: List[T => Process[Unit]]): Process[Unit] = members match {
      case member :: rest => member(t) >> loop(rest)
      case Nil => stop(())
    }
    loop(members)
  }
}
