trait FlowExamples {
  import flowlib._
  import ProcessUtil._
  import Producers._
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
