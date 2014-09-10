package flowlib


object Wiring extends Wiring

/**
 * A simplified process/gate wiring DSL which does not use Labels.
 * See also Graphs and Builder for a more complex alternative.
 */
trait Wiring {
  implicit class FlowNode[G, S](fn: G => S) {
    def ->:[G1](g: G1)(implicit ev: FlowIn[G1, G]): S = fn(ev.gate(g))
    def :->[G1](g: G1)(implicit ev: FlowOut[G1,G]): S = fn(ev.gate(g))
  }

  trait FlowIn[G1,G] { def gate(g: G1): G }
  trait FlowOut[G1,G] { def gate(g: G1): G }

  implicit def flowIn[A] = new FlowIn[Process[A], Process[A]] {
    def gate(g: Process[A]) = g
  }

  implicit def flowOut[A] = new FlowOut[A => Process[Unit], A => Process[Unit]] {
    def gate(g: A => Process[Unit]) = g
  }

  implicit def gateIn[A, B] = new FlowIn[Gate[A, B], Process[B]] {
    def gate(g: Gate[A, B]): Process[B] = Process.waitFor(g.take)
  }

  implicit def gateOut[A, B] = new FlowOut[Gate[A, B], A => Process[Unit]] {
    def gate(g: Gate[A, B]): A => Process[Unit] = a => Process.waitFor(k => g.offer(a)(k(())))
  }

  implicit def cbIn[A] = new FlowIn[(A => Unit) => Unit, Process[A]] {
    def gate(cb: (A => Unit) => Unit): Process[A] = Process.waitFor(cb)
  }

  implicit def cbOut[A] = new FlowOut[A => (=> Unit) => Unit, A => Process[Unit]] {
    def gate(cb: A => (=> Unit) => Unit): A => Process[Unit] = a => Process.waitFor(k => cb(a)(k(())))
  }
}
