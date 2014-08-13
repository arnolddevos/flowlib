package flowlib

import scala.collection.immutable.Queue
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

trait Transaction[T] {
  def transition: T => Option[T]
  def effect: T => Unit
}

trait Transactor[T] {
  def accept(r: Transaction[T]): Unit
}

object Transaction {

  val someAny: Any => Option[Any] = Some(_)

  private val noChangeAny = new PartialFunction[Any,Any] {
    def isDefinedAt(t :Any) = true
    def apply(t: Any) = t
    override def lift = someAny
  }

  def noChange[T] = noChangeAny.asInstanceOf[PartialFunction[T,T]]

  def assign[T](t: T) = new PartialFunction[T,T] {
    def isDefinedAt(x :T) = true
    def apply(x: T) = t
    override def lift: T => Option[T] = _ => Some(t)
  }

  val noEffect: Any => Unit = { _ => }

  def transaction[T](pf: PartialFunction[T, T])(k: T => Unit): Transaction[T] = {
    new Transaction[T] {
      val transition = pf.lift
      val effect = k
    }
  }
}

object Transactor {
  import Transaction._

  implicit class TranactorOps[T](val self: Transactor[T]) extends AnyVal {
    def transact(pf: PartialFunction[T, T])(k: T => Unit) =
      self.accept(transaction[T](pf)(k))
  }

  def apply[T](t0: T) = new Transactor[T] {

    private case class State(t: T, rs: List[Transaction[T]])
    private val cell = new AtomicReference(State(t0, Nil))

    def accept( r: Transaction[T] ): Unit = {

      type Tr = Transaction[T]
      type Th = (T, Tr)

      @tailrec
      def loop(t0: T, rs0: List[Tr], ns0: List[Tr], cs0: List[Th]): (T, List[Tr], List[Th]) = {
        ns0 match {
          case n0 :: ns1 =>
            n0.transition(t0) match {
              case Some(t1) =>
                loop(t1, Nil, rs0.reverse ::: ns1, (t0, n0) :: cs0)
              case _ =>
                loop(t0, n0 :: rs0, ns1, cs0)
            }
          case _ => (t0, rs0, cs0)
        }
      }

      @tailrec
      def run(): Unit = {
        val s0 @ State(t0, rs0) = cell.get

        r.transition(t0) match {

          case Some(t1) =>
            if(rs0.isEmpty) {
              // fast path if no pending retries
              val s1 = State(t1, Nil)
              if(cell.compareAndSet(s0, s1)) r.effect(t0)
              else run()
            }
            else {
              // general case
              val (tn, rsn, cs) = loop(t1, Nil, rs0.reverse, (t0, r) :: Nil)
              val s1 = State(tn, rsn)
              if(cell.compareAndSet(s0, s1))
                for((ti, ri) <- cs.reverse)
                  ri.effect(ti)
              else
                run()
            }

          case None =>
            // fast path for failed transaction
            val s1 = State(t0, r :: rs0)
            if(cell.compareAndSet(s0, s1)) {}
            else run()
        }
      }

      if( r.transition == someAny ){
        // fast path if no state transition
        r.effect(cell.get.t)
      }
      else run()
    }
  }
}
