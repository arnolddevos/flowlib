package flowlib

object ProcessUtil {
  import Process._

  def observe[U](p0: Process[U]): (Process[U], Responder[U]) = {
    val g = Gate.observable[U]()
    val p = p0 >>= { u => g accept Some(u); stop(u) }
    (p, g)
  }

  def join[A, B](pa: Process[A], pb: Process[B]): Process[(A,B)] = {
    val (p, g) = observe(pa)
    p & (pb >>= { b => Waiting(g, (a: A) => stop((a, b)))})
  }
}
