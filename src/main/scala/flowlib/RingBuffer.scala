package flowlib

class RingBuffer[T](backlog: Int) {
  private def maxBacklog = backlog * 2
  private case class State( buffer: Vector[T], seen: Long)
  private val state = Transactor(State(Vector.empty, 0l))

  def offer(t: T): Unit =
    state.transact {
      case State(buffer, seen) =>
        val r =
          if(buffer.length >= maxBacklog) buffer drop buffer.length-backlog-1
          else buffer
        State( r :+ t, seen + 1)
    } { _ => }

  def take( offset: Long )( k: (Long, T) => Unit): Unit =
    state.transact {
      case s @ State(_, seen) if offset < seen => s
    } {
      case State( buffer, seen ) =>
        val ix = (offset - seen + buffer.length max 0l).toInt
        k(ix - buffer.length + seen, buffer(ix))
    }
}
