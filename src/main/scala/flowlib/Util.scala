package flowlib
import scala.collection.immutable.{Queue, LinearSeq}
import java.util.concurrent.Executor

class BetterQ[T] private ( inner: Queue[T], val length: Int) {
  def this() = this(Queue.empty, 0)
  def enqueue(t :T) = new BetterQ(inner enqueue t, length + 1)
  def head = inner.head
  def tail = new BetterQ(inner.tail, length - 1)
  def isEmpty = length == 0
}

object Util {
  def clampLongtoInt(c: Long): Int = {
    if( c >= Int.MaxValue) Int.MaxValue
    else if( c <= Int.MinValue) Int.MinValue
    else c.toInt
  }
}

object Executors {

  trait StackType[A] {
    type Stack
    def empty: Stack
    def isEmpty(s: Stack): Boolean
    def enqueue(s: Stack, r: A): Stack
    def dequeue(s: Stack): (A, Stack)
  }

  object fifo extends StackType[Runnable] {
    type Stack = Queue[Runnable]
    def empty = Queue[Runnable]()
    def isEmpty(s: Stack) = s.isEmpty
    def enqueue(s: Stack, r: Runnable) = s enqueue r
    def dequeue(s: Stack) = s.dequeue
  }

  object lifo extends StackType[Runnable] {
    type Stack = List[Runnable]
    def empty = List[Runnable]()
    def isEmpty(s: Stack) = s.isEmpty
    def enqueue(s: Stack, r: Runnable) = r :: s
    def dequeue(s: Stack) = (s.head, s.tail)
  }

  def trampoline(stack: StackType[Runnable]) = new Executor {
    import stack._
    private var s = empty
    private var quiescent = true

    def execute( r0: Runnable ): Unit = {
      s = enqueue(s, r0)
      if(quiescent){
        quiescent = false
        while(! isEmpty(s)) {
          val (r, s1) = dequeue(s)
          s = s1
          r.run
        }
        quiescent = true
      }
    }
  }
}
