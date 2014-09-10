package flowlib

class Switch[K, T](backlog: Int) {
  import scala.collection.immutable.Queue
  import Transaction._

  private case class Message(key: K, value: T)
  private val state = Transactor(Queue[Message]())

  def input(key: K): T => (=> Unit) => Unit = { 
    t => k =>
      state.transact { 
        case q if q.size < backlog => q enqueue Message(key, t) 
      } {
        _ => k 
      }
  }

  def output(p: K => Boolean): (T => Unit) => Unit = { 

    object Dequeue {
      def unapply(q: Queue[Message]) = {
        val (a, b) = q span { case Message(key, _) => ! p(key) }
        if( b.isEmpty ) None else Some((a, b.head, b.tail))
      }
    }

    k =>
      state.transact { 
        case Dequeue(q, _, r ) => q ++ r 
      } { 
        case Dequeue(_, Message(_, t), _) => k(t) 
      }
  }
}
