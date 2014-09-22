package flowlib

trait Folder[+T] { parent =>

  def apply[S](s0: S)(f: (S, T) => Process[S]): Process[S]

  def map[U](g: T => U) = new Folder[U] {
    def apply[S](s0: S)(f: (S, U) => Process[S]) =
      parent.apply(s0)((s, t) => f(s, g(t)))
  }

  def flatMap[U](g: T => Folder[U]) = new Folder[U] {
    def apply[S](s0: S)(f: (S, U) => Process[S]) = {
      parent.apply(s0)((s, t) => g(t)(s)(f))
    }
  }

  def andThen[U](g: T => Process[U]) = new Folder[U] {
    def apply[S](s0: S)(f: (S, U) => Process[S]) =
      parent.apply(s0)((s, t) => g(t) >>= (f(s, _)))
  }
}

object Folder {
  import Process._

  def constant[T](t: T) = new Folder[T] {
    def apply[S](s0: S)(f: (S, T) => Process[S]): Process[S] = f(s0, t)
  }

  def empty = new Folder[Nothing] {
    def apply[S](s0: S)(f: (S, Nothing) => Process[S]): Process[S] = Process.stop(s0)
  }

  def stream[T](fold: Folder[T]): (Option[T] => Process[Unit]) => Process[Unit] = { sink =>
    fold(())((_, t) => sink(Some(t))) >> sink(None)
  }

  def unstream[T](source: Process[Option[T]]) = new Folder[T] {
    def apply[S](s0: S)(f: (S, T) => Process[S]): Process[S] = {
      def loop(s: S): Process[S] = source >>= {
        case Some(t) => f(s, t) >>= loop
        case None => stop(s)
      }
      loop(s0)
    }
  }
}
