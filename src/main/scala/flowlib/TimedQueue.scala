package flowlib

class TimedQueue[A](quota0: Int, period: Long) extends Gate[A, Option[A]] with Monitored {

  import scala.collection.immutable.Queue
  import Transaction._
  import Timing._

  private case class State( queue: Queue[A], backlog: Int, tick: Long, hold: Long)

  private val state = Transactor(State(Queue.empty, 0, 0l, 0l))

  def quota = quota0
  def backlog = state.snapshot.backlog
  def waiters = state.waiters

  def take( k: Option[A] => Unit): Unit = takeWithin(period)(k)

  def takeWithin(ms: Long)( k: Option[A] => Unit): Unit = {

    if( ms == Long.MaxValue ) takeSome(k)
    else if( ms <= 0 ) takeOption(k)
    else {

      val ticks = ms/period + 1
    
      state.transact { 
        case State(q, b, t, h) if b > 0 => State(q.tail, b-1, t, h)
        case State(q, b, t0, h0) => State(q, b, t0, h0 max t0 + ticks)
      } { 
        case State(q, b, _, _) if b > 0 => k(Some(q.head))
        case State(_, _, t0, h0) =>
          
          if( t0 >= h0 ) timer.schedule(pulse, period)

          state.transact { 
            case State(q, b, t, h) if b > 0 => State(q.tail, b-1, t, h)
            case s @ State(_, _, t, _) if t >= t0 + ticks => s
          } { 
            case State(q, b, _, _) if b > 0 => k(Some(q.head))
            case _ => k(None)
          }
      }
    }
  }

  def takeSome( k: Some[A] => Unit): Unit = 
    state.transact { 
      case State(q, b, t, h) if b > 0 => State(q.tail, b-1, t, h)
    } {
      case State(q, _, _, _) => k(Some(q.head))
    }

  def takeOption( k: Option[A] => Unit): Unit = 
    state.transact { 
      case State(q, b, t, h) if b > 0 => State(q.tail, b-1, t, h)
      case s => s
    } { 
      case State(q, b, _, _) if b > 0 => k(Some(q.head))
      case _ => k(None)
    }

  def offer(a: A)(k: => Unit): Unit =
    state.transact { 
      case State(q, b, t, h) if b < quota0 => State( q enqueue a, b + 1, t, h) 
    } { 
      _ => k 
    }

  private def pulse: Unit = 
    state.transact { 
      case State(q, b, t, h) => State(q, b, t + 1, h) 
    } { 
      case State(_, _, t, h) => if( t + 1 < h) timer.schedule(pulse, period)
    }
}
