package flowlib
import scala.language.higherKinds
import scala.collection.immutable.{Queue, LinearSeq}
import java.util.concurrent.Executor

class BetterQ[T] private ( inner: Queue[T], val length: Int) {
  def this() = this(Queue.empty, 0)
  def enqueue(t :T) = new BetterQ(inner enqueue t, length + 1)
  def head = inner.head
  def tail = new BetterQ(inner.tail, length - 1)
  def isEmpty = length == 0
}

trait NaturalMap[K[_], V[_]] { parent =>

  protected val underlying: Map[K[_], V[_]]

  def get[T]( key: K[T]) = 
    underlying.get(key).asInstanceOf[Option[V[T]]]
  
  def updated[T]( key: K[T], value: V[T]) = new NaturalMap[K, V] {
    protected val underlying = parent.underlying.updated(key, value)
  }
}

object NaturalMap { 
  def empty[K[_], V[_]] = new NaturalMap[K, V] { 
    protected val underlying = Map.empty[K[_], V[_]]
  }
  def unsafe[K[_], V[_]]( base: Map[K[_], V[_]]) = new NaturalMap[K, V] { 
    protected val underlying = base
  }
}

trait DisjointSets[T] {
  def sets: Map[T, Set[T]]
  def find(t: T): Option[(T, Set[T])] = sets find { case (_, ts) => ts contains t }
  def add(t: T): DisjointSets[T] = union(t, t)
  def union(t1: T, t2: T): DisjointSets[T] = {
    val sets1 = (find(t1), find(t2)) match {
      case (Some((r1, s1)), Some((r2, s2))) => sets + (r1 -> (s1 union s2)) - r2
      case (Some((r1, s1)), None) => sets + (r1 -> (s1 + t2))
      case (None, Some((r2, s2))) => sets + (r2 -> (s2 + t1))
      case (None, None) => sets + (t1 -> Set(t1, t2))
    }
    new DisjointSets[T] { val sets = sets1 }
  }
}

object DisjointSets {
  def empty[T] = new DisjointSets[T] { def sets = Map.empty[T, Set[T]] }
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
