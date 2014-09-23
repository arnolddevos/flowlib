package flowlib


object Wiring extends Wiring

/**
 * A simplified process/gate wiring DSL which does not use Labels.
 * See also Graphs and Builder for a more complex alternative.
 */
trait Wiring {
  import Process.waitFor, ProcessUtil.fanout

  implicit class FlowNode[G, S](fn: G => S) {
    def ->:[G1](g: G1)(implicit ev: FlowIn[G1, G]): S = fn(ev.gate(g))
    def :->[G1](g: G1)(implicit ev: FlowOut[G1,G]): S = fn(ev.gate(g))
  }

  def tee[G, A](gs: G*)(implicit e: FlowOut[G, A => Process[Unit]]) = 
    fanout(gs.toList map (e.gate(_)))

  def right[G, G1, B](g :G)(implicit e: FlowOut[G, G1], f: G1 <:< (Either[Nothing, B] => Process[Unit])): B => Process[Unit] = 
    b => f(e.gate(g))(Right(b))

  def left[G, G1, A](g :G)(implicit e: FlowOut[G, G1], f: G1 <:< (Either[A, Nothing] => Process[Unit])): A => Process[Unit] = 
    a => f(e.gate(g))(Left(a))

  trait FlowIn[G1,G] { def gate(g: G1): G }

  trait FlowOut[G1,G] { def gate(g: G1): G }

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
