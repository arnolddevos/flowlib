package flowlib

import scala.language.higherKinds
import ProcessUtil._
import Producers._

object Flow extends Flow

/**
 * A simplified process/gate wiring DSL which does not use Labels, take N.
 */
trait Flow {

  implicit class FlowNode[N]( node: N) {
    def ->:[S, M](input: S)(implicit connect: FlowIn[S, N, M]): M = connect(input, node)
    def :->[S, M](output: S)(implicit connect: FlowOut[S, N, M]): M = connect(output, node)
    def :<-[S, M](input: S)(implicit connect: FlowIn[S, N, M]): M = connect(input, node)
  }

  trait FlowIn[-S, -N, +M] extends ((S, N) => M)
  trait FlowOut[-S, -N, +M] extends ((S, N) => M)

  def flowIn[S, N, M](f: (S, N) => M) = new FlowIn[S, N, M] {
    def apply(s: S, n: N): M = f(s, n)
  }

  def flowOut[S, N, M](f: (S, N) => M) = new FlowOut[S, N, M] {
    def apply(s: S, n: N): M = f(s, n)
  }

  implicit def connectSink[A, M]: FlowOut[Sink[A], Sink[A] => M, M] = flowOut((s, n) => n(s))
  implicit def connectSource[A, M]: FlowIn[Source[A], Source[A] => M, M] = flowIn((s, n) => n(s))
  implicit def connectGateOut[A, M]: FlowOut[Gate[A, Any], Sink[A] => M, M] = flowOut((s, n) => n(sendTo(s)))
  implicit def connectGateIn[A, M]: FlowIn[Gate[Nothing, A], Source[A] => M, M] = flowIn((s, n) => n(takeFrom(s)))

  def tee[A]: Sink[A] => Sink[A] => Sink[A] = s1 => s2 => fanout(List(s1, s2))

  implicit def connectSinks[S, N, M](implicit c: FlowOut[S, N, M]): FlowOut[S, List[N], List[M]] =
    flowOut((s, ns) => ns map (n => c(s, n)))

  implicit def connectSources[S, N, M](implicit c: FlowIn[S, N, M]): FlowIn[S, List[N], List[M]] =
    flowIn((s, ns) => ns map (n => c(s, n)))

  implicit def connectEduction[A, R[_], S](implicit e: Educible[R], c: FlowOut[S, Sink[A] => Sink[A], Sink[A]]): FlowOut[S, R[A], Process[Unit]] = {
    flowOut {
      (s, ra) => 
        val o = c(s, identity)
        val p = emit(ra)
        p(o)
    }
  }

  implicit def connectReduction1[A, R]: FlowIn[Source[A], Reducer[A, R], Process[R]] = {
    flowIn {
      (s, f) =>
        val p = absorb(f)
        p(s)
    }
  }

  implicit def connectReduction2[A, R]: FlowIn[Gate[Nothing, A], Reducer[A, R], Process[R]] = {
    flowIn {
      (g, f) =>
        val s = takeFrom(g)
        val p = absorb(f)
        p(s)
    }
  }
}

trait FlowExamples {
  import Gate._
  import Flow._

  type A
  type A1 <: A
  type B
  type C
  type C1 <: C

  def s1: Source[A]
  def s2: Sink[B]
  def sa: Sink[Any]
  def c1: Channel[A]
  def c2: Channel[C1]
  def g1: Gate[B, A1]
  def p1: Source[A] => Sink[B] => Process[Nothing]
  def p2: Source[C] => Source[A] => Sink[B] => Process[Nothing]
  def l1: List[B]
  def pr1: Producer[A]
  def r1: Reducer[A, List[A]] = reducer(List[A]())((la, a: A) => Process.stop(a :: la))

  def ps: Process[Any] = (
    s1 ->: p1 :-> s2 & 
    c1 ->: p1 :-> s2 & 
    g1 ->: p1 :-> g1 & 
    s1 ->: p1 :-> g1 & 
    s1 ->: c2 ->: p2 :-> s2  &
    c2 ->: p2 :<- s1 :-> (tee[B] :-> s2 :-> sa) &
    l1 :-> s2 &
    pr1 :-> c1 & 
    s1 ->: r1 &
    g1 ->: r1
  )

}
