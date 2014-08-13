package flowlib

import scala.collection.immutable.Queue

class Switch[K, T](capacity: Int) {
  import Transaction._

  private case class Message(key: K, value: T)

  private val state = Transactor(Queue[Message]())
  private val semaphore = Gate.semaphore(capacity)

  def input(key: K): Responder[Acceptor[T]] = semaphore map { _ => 
    new Acceptor[T] {
      def accept(t: T) = 
        state.transact { case q => q enqueue Message(key, t) } { noEffect }
    }
  }

  def output(p: K => Boolean): Responder[T] = new Responder[T] {
    def respond( k: T => Unit ) = {
      state.transact { case Dequeue(q, _, r ) => q ++ r } { case Dequeue(_, Message(_, t), _) => k(t) }
      semaphore accept 1
    }

    private object Dequeue {
      def unapply(q: Queue[Message]) = {
        val (a, b) = q span { case Message(key, _) => ! p(key) }
        if( b.isEmpty ) None else Some((a, b.head, b.tail))
      }
    }
  }

  def control: Acceptor[Long] = semaphore
}
