package flowlib

trait Gate[-S, +T] {
  def take(k: T => Unit): Unit
  def offer(s: S)(k: => Unit): Unit
  def signal(s: S) = offer(s)(())
}

object Gate {
  import scala.collection.immutable.Queue
  import Transaction._

  def semaphore(v0: Long) = new Gate[Long, Long] {
    private val state = Transactor(v0)

    // P or wait
    def take( k: Long => Unit): Unit =
      state.transact { case v if v > 0 => v -1 } { k }

    // V or signal
    def offer(i: Long)(k: => Unit): Unit =
      state.transact { case v => v + i } { _ => k }
  }

  def barrier() = new Gate[Unit, Long] {
    private val state = Transactor(0l)

    def take( k: Long => Unit): Unit =
      state.transact { noChange } { v0 =>
        state.transact { case v1 if v1 > v0 => v1 } { k }
      }

    def offer(u: Unit)(k: => Unit): Unit =
      state.transact { case v0 => v0 + 1 } { _ => k }
  }

  def observable[T](initial: Option[T]=None) = new Gate[Option[T], T] {
    private val state = Transactor(initial)

    def take( k: T => Unit): Unit =
      state.transact { case ot @ Some(_) => ot } { _ foreach k }

    def offer(ot: Option[T])(k: => Unit): Unit =
      state.transact { assign(ot) } { _ => k }
  }

  def latch[T] = new Gate[T, T] {
    private val state = Transactor(None: Option[T])

    def take( k: T => Unit): Unit =
      state.transact { case ot @ Some(_) => ot } { _ foreach k }

    def offer(t: T)(k: => Unit): Unit =
      state.transact { 
        case ot @ Some(_) => ot 
        case None => Some(t) 
      } { _ => k }
  }

  trait Channel[T] extends Gate[T, T] with Monitored 

  def channel[T](quota0: Int) = new Channel[T] {
    private val state = Transactor(Queue[T]())

    def quota = quota0
    def backlog = state.snapshot.length
    def waiters = state.waiters

    def take( k: T => Unit): Unit =
      state.transact { case q if ! q.isEmpty => q.tail } { q => k(q.head) }

    def offer(t: T)(k: => Unit): Unit =
      state.transact { case q if q.length < quota => q enqueue t } { _ => k }
  }

  def wye[T1, T2] = new Gate[Either[T1, T2], (T1, T2)] {

    trait State
    case object Empty extends State
    case class Half(t1: T1) extends State
    case class Full(t12: (T1,T2)) extends State

    private val state = Transactor(Empty: State)

    def take(k: ((T1, T2)) => Unit): Unit =
      state.transact { case Full(_) => Empty } { case Full(t12) => k(t12) }

    def offer(et: Either[T1,T2])(k: => Unit): Unit = et match {
      case Left(t1) =>
        state.transact { case Empty => Half(t1) } { _ => k }
      case Right(t2) =>
        state.transact { case Half(t1) => Full((t1, t2)) } { _ => k }
    }
  }
}
