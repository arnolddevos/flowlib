package flowlib


object Wiring extends Wiring

/**
 * A simplified process/gate wiring DSL which does not use Labels.
 * See also Graphs and Builder for a more complex alternative.
 */
trait Wiring {
  import Process.{waitFor, stop}, ProcessUtil.fanout
  import Gate.Channel

  type Source[+T]       = Process[T]
  type Sink[-T]         = T => Process[Unit]

  implicit class FlowNode[G2, S](fn: G2 => S) {
    def ->:[G1, G <: G2](g: G1)(implicit ev: FlowIn[G1, G]): S = fn(ev.gate(g))
    def :->[G1, G <: G2](g: G1)(implicit ev: FlowOut[G1,G]): S = fn(ev.gate(g))
  }

  implicit class FlowNodes[G2, S](fns: List[G2 => S]) {
    def ->:[G1, G <: G2](g: G1)(implicit ev: FlowIn[G1, G]): List[S] = 
      fns map(fn => fn(ev.gate(g)))
    def :->[G1, G <: G2](g: G1)(implicit ev: FlowOut[G1,G]): List[S] = 
      fns map(fn => fn(ev.gate(g)))
  }

  implicit class FlowProcesses[A](ps: List[Process[A]]) {
    def !:( name: String): List[Process[A]] = ps map (name !: _)
  } 

  def parallel[A](ps: List[Process[A]]) = ps reduce ( _ & _ )

  def tee[G1, G2, A](g1: G1, g2: G2)(implicit e1: FlowOut[G1, Sink[A]], e2: FlowOut[G2, Sink[A]]) = 
    fanout(List(e1.gate(g1), e2.gate(g2)))

  def compose[G, A, B](g: G)(f: A => B)(implicit e: FlowOut[G, Sink[B]]): Sink[A] = 
    e.gate(g) compose f

  def right[G1, G2, B](g :G1)(implicit e: FlowOut[G1, G2], f: G2 <:< Sink[Either[Nothing, B]]): Sink[B] = 
    b => f(e.gate(g))(Right(b))

  def left[G1, G2, A](g :G1)(implicit e: FlowOut[G1, G2], f: G2 <:< Sink[Either[A, Nothing]]): Sink[A] = 
    a => f(e.gate(g))(Left(a))

  trait FlowIn[G1,G2] { def gate(g: G1): G2 }

  trait FlowOut[G1,G2] { def gate(g: G1): G2 }

  implicit def flowIn[A] = new FlowIn[Source[A], Source[A]] {
    def gate(g: Source[A]) = g
  }

  implicit def flowOut[A] = new FlowOut[Sink[A], Sink[A]] {
    def gate(g: Sink[A]) = g
  }

  implicit def gateIn[A, B] = new FlowIn[Gate[A, B], Source[B]] {
    def gate(g: Gate[A, B]): Source[B] = waitFor(g.take)
  }

  implicit def gateOut[A, B] = new FlowOut[Gate[A, B], Sink[A]] {
    def gate(g: Gate[A, B]): Sink[A] = a => waitFor(k => g.offer(a)(k(())))
  }

  implicit def chanIn[A] = new FlowIn[Channel[A], Source[A]] {
    def gate(g: Channel[A]): Source[A] = waitFor(g.take)
  }

  implicit def chanOut[A] = new FlowOut[Channel[A], Sink[A]] {
    def gate(g: Channel[A]): Sink[A] = a => waitFor(k => g.offer(a)(k(())))
  }

  implicit def cbIn[A] = new FlowIn[(A => Unit) => Unit, Source[A]] {
    def gate(cb: (A => Unit) => Unit): Source[A] = waitFor(cb)
  }

  implicit def cbOut[A] = new FlowOut[A => (=> Unit) => Unit, Sink[A]] {
    def gate(cb: A => (=> Unit) => Unit): Sink[A] = a => waitFor(k => cb(a)(k(())))
  }
}
