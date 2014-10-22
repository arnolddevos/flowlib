object Transduction extends App {
  import flowlib._
  import Transducers._

  {
    val l = List(1, 2, 3)
    val f = reducer[Int, Int](0)(_ + _)
    val s = reduce(l)(f) // s == 6
    println(s)
  }
  {
    val l = List(1.0, 2.0, 3.0)
    def f(s: Double, a: Double) = s*0.9 + a*0.1
    val r = reducer(0.0)(f)
    val s = reduce(l)(r) // s == 
    println(s)
  }
  {
    val l = List(1.0, 2.0, 3.0)
    def f(s: Double, a: Double) = s*0.9 + a*0.1
    val r1 = reducer(0.0)(f)
    val r2 = takeN(2)(r1)
    val s = reduce(l)(r2) // s == 
    println(s)
  }
}