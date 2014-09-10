package flowlib

trait Folder[T] { parent =>
  def apply[S](s0: S)(f: (S, T) => Process[S]): Process[S]
  def map[U](g: T => U) = new Folder[U] {
    def apply[S](s0: S)(f: (S, U) => Process[S]) =
      parent.apply(s0)((s, t) => f(s, g(t)))
  }
}

object Folder {
  import Process._

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
