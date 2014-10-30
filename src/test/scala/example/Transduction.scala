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
  {
    val l = List(1.0, 2.0, 3.0)
    def f(s: Double, a: Double) = s*0.9 + a*0.1
    val r = reducer(0.0)(f)
    def take2[X] = takeN[X](2)
    val s = transduce(l)(take2, r) // s == 
    println(s)
  }
  {
    val l = List(1.0, 2.0, 3.0)
    def f(s: Double, a: Double) = s*0.9 + a*0.1
    val r = reducer(0.0)(f)
    def take2[X] = takeN[X](2)
    def square = mapper((x: Double) => x*x) 
    val s = transduce(l)(take2 compose square, r) // s == 
    println(s)
  }
  {
    val l = List("1.0", "2.0", "3.0")
    val r = reducer(0.0)((s, a: Double) => s*0.9 + a*0.1)
    val asDouble = mapper((s: String) => s.toDouble)
    def take2[X] = takeN[X](2)
    def square = mapper((x: Double) => x*x) 
    val s = transduce(l)(asDouble compose take2 compose square, r) // s == 
    println(s)
  }
  {
    val l = List("1.0", "2.0", "3.0")
    val r = reducer(0.0)((s, a: Double) => s*0.9 + a*0.1)
    val asDouble = mapper((s: String) => s.toDouble)
    def take2[X] = takeN[X](2)
    def square = mapper((x: Double) => x*x) 
    val s = transduce(l)(take2 compose asDouble compose take2 compose square, r) // s == 
    println(s)
  }
}
