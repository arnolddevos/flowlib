package flowlib


object Wiring extends Wiring

/**
 * A simplified process/gate wiring DSL which does not use Labels.
 * See also Graphs and Builder for a more complex alternative.
 */
trait Wiring {
  import Process.waitFor, ProcessUtil.fanout

  implicit class FlowNode[G2, S](fn: G2 => S) {
    def ->:[G1, G <: G2](g: G1)(implicit ev: FlowIn[G1, G]): S = fn(ev.gate(g))
    def :->[G1, G <: G2](g: G1)(implicit ev: FlowOut[G1,G]): S = fn(ev.gate(g))
  }

  def tee[G, A](gs: G*)(implicit e: FlowOut[G, A => Process[Unit]]) = 
    fanout(gs.toList map (e.gate(_)))

  def compose[G, A, B](f: B => A)(g: G)(implicit e: FlowOut[G, A => Process[Unit]]): B => Process[Unit] = 
    e.gate(g) compose f

  def right[G1, G2, B](g :G1)(implicit e: FlowOut[G1, G2], f: G2 <:< (Either[Nothing, B] => Process[Unit])): B => Process[Unit] = 
    b => f(e.gate(g))(Right(b))

  def left[G1, G2, A](g :G1)(implicit e: FlowOut[G1, G2], f: G2 <:< (Either[A, Nothing] => Process[Unit])): A => Process[Unit] = 
    a => f(e.gate(g))(Left(a))

  trait FlowIn[G1,G2] { def gate(g: G1): G2 }

  trait FlowOut[G1,G2] { def gate(g: G1): G2 }

  implicit def flowIn[A] = new FlowIn[Process[A], Process[A]] {
    def gate(g: Process[A]) = g
  }

  implicit def flowOut[A] = new FlowOut[A => Process[Unit], A => Process[Unit]] {
    def gate(g: A => Process[Unit]) = g
  }

  implicit def gateIn[A, B] = new FlowIn[Gate[A, B], Process[B]] {
    def gate(g: Gate[A, B]): Process[B] = waitFor(g.take)
  }

  implicit def gateOut[A, B] = new FlowOut[Gate[A, B], A => Process[Unit]] {
    def gate(g: Gate[A, B]): A => Process[Unit] = a => waitFor(k => g.offer(a)(k(())))
  }

  implicit def cbIn[A] = new FlowIn[(A => Unit) => Unit, Process[A]] {
    def gate(cb: (A => Unit) => Unit): Process[A] = waitFor(cb)
  }

  implicit def cbOut[A] = new FlowOut[A => (=> Unit) => Unit, A => Process[Unit]] {
    def gate(cb: A => (=> Unit) => Unit): A => Process[Unit] = a => waitFor(k => cb(a)(k(())))
  }
}
