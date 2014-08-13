package flowlib
import Labels._

sealed trait Process[+U]

object Process {

  // states forming a trampoline
  case class Ready[U]( step: () => Process[U] ) extends Process[U]
  case class Complete[U](u: U) extends Process[U]

  // failed state triggers error handling
  case class Failed(e: Throwable) extends Process[Nothing]

  // states for concurrent processes
  case class Waiting[T, U]( gate: Responder[T], step: T => Process[U] ) extends Process[U]
  case class Asynchronous[U]( step: () => Process[U] ) extends Process[U]

  // states for process combinators
  case class Sequential[V, U]( process: Process[V], step: V => Process[U]) extends Process[U]
  case class Parallel[V, U]( process: Process[V], step: Process[U]) extends Process[U]

  // naming and linking processes
  case class Labelled[T, U]( label: Label[T], step: T => Process[U] ) extends Process[U]
  case class Named[U](name: String, step: Process[U]) extends Process[U] {
    override def toString = s"Process($name)"
  }

  def process[U]( step: => Process[U]): Process[U] = Asynchronous(() => step)

  def receive[T, U]( gate: Responder[T])( step: T => Process[U]): Process[U] =
    Waiting(gate, (t: T) => Asynchronous(() => step(t)))

  def send[T, U]( gate: Responder[Acceptor[T]], t: T)( step: => Process[U]): Process[U] =
    Waiting(gate, {i: Acceptor[T] => i accept t; Asynchronous(() => step)})

  def continue[U]( step: => Process[U]): Process[U] = Ready(() => step)

  def stop[U](u: U): Process[U] = Complete(u)

  def fail(message: String, cause: Throwable=null): Process[Nothing] =
    Failed(new RuntimeException(message, cause))

  implicit class ProcessOps[U](p0: Process[U]) {
    def map[V]( f: U => V ): Process[V] = flatMap(u => Complete(f(u)))
    def flatMap[V]( step: U => Process[V]): Process[V] = Sequential(p0, step)
    def >>=[V]( step: U => Process[V]): Process[V] = Sequential(p0, step)
    def >>[V]( step: => Process[V]): Process[V] = Sequential(p0, (_:U) => step)
    def &[V](p1: Process[V]): Process[V] = Parallel(p0, p1)
    def !:(name: String): Process[U] = Named(name, p0)
    def run() = (new DefaultSite {}) run p0
  }
}
