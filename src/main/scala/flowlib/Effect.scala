package flowlib
import Process.{WaitingAsync, Complete, Sequential, Failed}

sealed trait Effect

object Effect {
  trait Waits extends Effect
  trait Fails extends Effect
}

case class Checked[+U, +E <: Effect](p: Process[U]) extends AnyVal {
  def >>=[V, E1 >: E <: Effect]( step: U => Checked[V, E1]) = Checked[V, E1](Sequential(p, step andThen (_.p)))
}

object Checked {
  def combine[U, T, S, E <: Effect](u: Checked[U, E], t: Checked[T, E])(f: (Process[U], Process[T]) => Process[S]): Checked[S, E] = 
    Checked[S, E](f(u.p, t.p))

  import Effect._

  def waitFor[T]( respond: (T => Unit) => Unit) = Checked[T, Waits](WaitingAsync(respond))

  def stop[U](u: U) = Checked[U, Nothing](Complete(u))

  def fail(message: String, cause: Throwable=null) =
    Checked[Nothing, Fails](Failed(new RuntimeException(message, cause)))

}
