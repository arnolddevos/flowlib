package flowlib

import scala.collection.immutable.Queue

class Channel[T](capacity: Int) {
  import Gate._

  private val q = queue[T]
  private val s = semaphore(capacity)

  val input: Responder[Acceptor[T]] = s map (_ => q)

  val output = new Responder[T] {
    def respond( k: T => Unit ) = {
      s accept 1
      q respond k
    }
  }

  def control: Acceptor[Long] = s
}
