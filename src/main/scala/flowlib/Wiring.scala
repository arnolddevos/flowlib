package flowlib


object Wiring extends Wiring

/**
 * A simplified process/gate wiring DSL which does not use Labels.
 * See also Graphs and Builder for a more complex alternative.
 */
trait Wiring {

  /*import language.higherKinds*/
  import flowlib.{Process, Responder, Acceptor, Gate}

  trait Term[T, U] {
    def ps(t: T): List[Process[U]]
  }

  trait GTerm[T, G, U] extends Term[T, U] {
    def gate(t: T): G
  }

  trait FTerm[T, G, S, U] extends Term[T, U] {
    def gss(t: T): List[G => S]
  }

  trait Connector[T1, T2, T3] {
    def connect(t1: T1, t2: T2): T3
  }

  implicit class InputOp[T1, A, S, U](t1 :T1)(implicit ft: FTerm[T1, Responder[A], S, U]) {
    def =>:[T2, T3](t2: T2)(implicit ct: Connector[T1, T2, T3]) = ct.connect(t1, t2)
  }

  implicit class OutputAOp[T1, A, S, U](t1: T1)(implicit ft: FTerm[T1, Acceptor[A], S, U]) {
    def :=>[T2, T3](t2: T2)(implicit ct: Connector[T1, T2, T3]) = ct.connect(t1, t2)
  }

  implicit class OutputROp[T1, A, S, U](t1: T1)(implicit ft: FTerm[T1, Responder[Acceptor[A]], S, U]) {
    def :=>[T2, T3](t2: T2)(implicit ct: Connector[T1, T2, T3]) = ct.connect(t1, t2)
  }

  implicit class ParallelFOp[T1, G, S, U1](t1: T1)(implicit ft1: FTerm[T1, G, S, U1]) {
    def &[T2, U2 >: U1](t2: T2)(implicit ft2: FTerm[T2, G, S, U2]) =
      new FValue(ft1.ps(t1) ::: ft2.ps(t2), ft1.gss(t1) ::: ft2.gss(t2))
  }

  implicit class ParallelPOp[U1](t1: PValue[U1]) {
    def &[U2 >: U1](t2: PValue[U2]) = PValue(t1.ps ::: t2.ps)
  }

  def wiring[U](t: PValue[U]): Process[U] = t.ps.reduce(_ & _)

  case class PValue[+U](ps: List[Process[U]])

  case class GValue[+G, +U](ps: List[Process[U]], gate: G)

  case class FValue[-G, +S, +U](ps: List[Process[U]], gss: List[G => S])

  implicit def gTerm1[G, U] = new GTerm[GValue[G, U], G, U] {
    def gate(t: GValue[G, U]) = t.gate
    def ps(t: GValue[G, U]) = t.ps
  }

  implicit def gTerm2[A] = new GTerm[Acceptor[A], Acceptor[A], Nothing] {
    def gate(t: Acceptor[A]) = t
    def ps(t: Acceptor[A]): List[Process[Nothing]] = Nil
  }

  implicit def gTerm3[A] = new GTerm[Responder[A], Responder[A], Nothing] {
    def gate(t: Responder[A]) = t
    def ps(t: Responder[A]): List[Process[Nothing]] = Nil
  }

  implicit def gTerm4[A, U] = new GTerm[(Process[U], Responder[A]), Responder[A], U] {
    def gate(t: (Process[U], Responder[A])) = t._2
    def ps(t: (Process[U], Responder[A])) = t._1 :: Nil
  }

  implicit def fTerm1[G, S, U] = new FTerm[FValue[G, S, U], G, S, U] {
    def gss(t:  FValue[G, S, U]) = t.gss
    def ps(t:  FValue[G, S, U]) = t.ps
  }

  implicit def fTerm2[G, S] = new FTerm[G => S, G, S, Nothing] {
    def gss(t:  G => S) = t :: Nil
    def ps(t:  G => S): List[Process[Nothing]] = Nil
  }

  implicit def connector1[T1, T2, G, U >: U1, U1 >: U2, U2](implicit ft: FTerm[T1, G, Process[U], U1], gt: GTerm[T2, G, U2]) =
    new Connector[T1, T2, PValue[U]] {
      def connect(t1: T1, t2: T2) = PValue(ft.gss(t1).map {f => f(gt.gate(t2))} ::: ft.ps(t1) ::: gt.ps(t2))
    }

  implicit def connector2[T1, T2, G1, G2, S, U1 >: U2, U2](implicit ft: FTerm[T1, G1, G2 => S, U1], gt: GTerm[T2, G1, U2]) =
    new Connector[T1, T2, FValue[G2, S, U1]] {
      def connect(t1: T1, t2: T2) = FValue(ft.ps(t1) ::: gt.ps(t2), ft.gss(t1).map {f => f(gt.gate(t2))})
    }
}
