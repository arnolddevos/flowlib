package flowlib

object ProcessUtil {
  import Process._

  type Source[+T]       = Process[T]
  type Sink[-T]         = T => Process[Unit]

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

  def foreach[A](la: List[A])( f: A => Process[Any] ): Process[Unit] = {
    la match {
      case a :: la1 => f(a) >> foreach(la1)(f)
      case Nil => stop(())
    }
  }

  def traverseProcess[A, B](la: List[A])(f: A => Process[B]): Process[List[B]] = {
    def loop(la: List[A], lb: List[B]): Process[List[B]] =
      la match {
        case a :: la1 => f(a) >>= (b => loop(la1, b :: lb))
        case Nil => stop(lb.reverse)
      }
    loop(la, Nil)
  }

  def forever(p: Process[Any]): Process[Nothing] =
    p >> forever(p)

  def applyForever[A](f: A => Process[Any]): Source[A] => Process[Nothing] =
    input => forever(input >>= f)

  def mapForever[A, B](f: A => Process[B]): Source[A] => Sink[B] => Process[Nothing] =
    input => output => forever(input >>= f >>= output)

  def foldForever[S, A](z: S)(f: (S, A) => Process[S]): Source[A] => Process[Nothing] = {
    input =>
      def loop(s: S): Process[Nothing] = input >>= (f(s, _)) >>= loop
      loop(z)
  }

  def foldMapForever[S, A, B](z: S)(f: (S, A) => Process[(S, B)]): Source[A] => Sink[B] => Process[Nothing] = {
    input => output =>
      def loop(s0: S): Process[Nothing] = input >>= (f(s0, _)) >>= {
        case (s1, b) => output(b) >> loop(s1)
      }
      loop(z)
  }

  def cat[A]: Source[A] => Sink[A] => Process[Nothing] =
    source => sink => forever(source >>= sink)

  def valve[A]: Source[Any] => Source[A] => Sink[A] => Process[Nothing] =
    control => source => sink => forever(control >> (source >>= sink))

  def sendTo[A](g: Gate[A, Any]): Sink[A] =
    a => waitDone(g offer a)

  def takeFrom[A](g: Gate[Nothing, A]): Source[A] =
    waitFor(g.take)

  def fanout[T]( sinks: List[Sink[T]]): Sink[T] =
    t => foreach(sinks){ sink => sink(t) }

  implicit class ProcessesOp[A](ps: List[Process[A]]) {
    def !:(name: String): List[Process[A]] = ps map (name !: _)
  }

  def parallel[A](ps: List[Process[A]]) = ps reduce ( _ & _ )

  def bracket[R, T](acquire: Process[R])(use: R => Process[T])(dispose: R => Process[Unit]): Process[T] =
    acquire >>= (r => (use(r) recoverWith dispose(r)) >>= (t => dispose(r) >> stop(t)))
}
