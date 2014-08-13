package flowlib

import scala.collection.immutable.Queue

trait Gate[S, T] extends Acceptor[S] with Responder[T]

object Gate {
  import Transaction._

  def semaphore(v0: Long) = new Gate[Long, Long] {
    private val state = Transactor(v0)

    def respond( k: Long => Unit): Unit =
      state.transact { case v if v > 0 => v -1 } { k }

    def accept(i: Long): Unit =
      state.transact { case v => v + i } { noEffect }
  }

  def barrier() = new Gate[Unit, Long] {
    private val state = Transactor(0l)

    def respond( k: Long => Unit): Unit =
      state.transact { noChange } { v0 =>
        state.transact { case v1 if v1 > v0 => v1 } { k }
      }

    def accept(u: Unit): Unit =
      state.transact { case v0 => v0 + 1 } { noEffect }
  }

  def observable[T](initial: Option[T]=None) = new Gate[Option[T], T] {
    private val state = Transactor(initial)

    def respond( k: T => Unit): Unit =
      state.transact { case ot @ Some(_) => ot } { _ foreach k }

    def accept(ot: Option[T]): Unit =
      state.transact { assign(ot) } { noEffect }
  }

  def queue[T]() = new Gate[T, T] {
    private val state = Transactor(Queue[T]())

    def respond( k: T => Unit): Unit =
      state.transact { case q if ! q.isEmpty => q.tail } { q => k(q.head) }

    def accept(t: T): Unit =
      state.transact { case q => q enqueue t } { noEffect }
  }

  def pair[T1, T2] = new Gate[Either[T1, T2], (T1, T2)] {

    trait State
    case object Empty extends State
    case class Half(t1: T1) extends State
    case class Full(t12: (T1,T2)) extends State

    private val state = Transactor(Empty: State)

    def accept(et: Either[T1,T2]): Unit = et match {
      case Left(t1) =>
        state.transact { case Empty => Half(t1) } { noEffect }
      case Right(t2) =>
        state.transact { case Half(t1) => Full((t1, t2)) } { noEffect }
    }

    def respond(k: ((T1, T2)) => Unit): Unit =
      state.transact { case Full(_) => Empty } { case Full(t12) => k(t12) }
  }
}
