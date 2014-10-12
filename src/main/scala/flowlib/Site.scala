
package flowlib

import annotation.tailrec
import util.control.NonFatal
import java.util.concurrent.{ExecutorService, ForkJoinPool}

trait Site {
  import Process._

  def success[U](p0: Process[U], u: U): Unit
  def failure[U](p0: Process[U], e: Throwable): Unit
  def executor: ExecutorService

  private def async[V, U](p0: Process[V], u: U)(k: U => Unit): Unit = {
    executor execute new Runnable {
      def run: Unit = {
        try { k(u) }
        catch { case NonFatal(e) => failure(p0, e) }
      }
    }
  }

  @tailrec
  private def bounce[V, U](p0: Process[V], p: Process[U])(k: U => Unit): Unit = {
    p match {

      // most popular cases it is supposed
      case WaitingAsync(respond)   => respond((async(p0, _)(k)))
      case Complete(u)             => k(u)

      // these cases prevent stack overflow
      case Sequential(Complete(x), step)
        => bounce(p0, step(x))(k)
      case Sequential(Sequential(p1, step1), step2)
        => bounce(p0, Sequential(p1, (x: Any) => Sequential(step1(x), step2)))(k)

      // flatmap that shit
      case Sequential(p1, step)    => push(p0, p1)(t => push(p0, step(t))(k))

      // the remaining cases
      case Ready(step)             => bounce(p0, step())(k)
      case Waiting(respond)        => respond(k)
      case Asynchronous(step)      => async(p0, ())(_ => push(p0, step())(k))
      case Parallel(p1)            => run(p1); k(().asInstanceOf[U])
      case Named(_, p1)            => bounce(p0, p1)(k)
      case Failed(e)               => failure(p0, e)
    }
  }

  private def push[V, U](p0: Process[V], p: Process[U])(k: U => Unit): Unit =
    bounce(p0, p)(k)

  final def run[U](p0: Process[U]): Unit = bounce(p0, p0)(success(p0, _))
}

class DefaultSite extends Site with Monitored {
  val executor = new ForkJoinPool
  def success[U](p0: Process[U], u: U): Unit =
    println(s"Completed $p0 with: $u")
  def failure[U](p0: Process[U], e: Throwable): Unit = {
    println(s"Failed $p0 with: ${formatException(e)}")
    executor.shutdown
  }
  def formatException(e: Throwable) = {
    val c = e.getCause
    if(c != null) s"$e cause: $c" else e.toString
  }

  def backlog = executor.getActiveThreadCount
  def quota = executor.getParallelism
  def waiters = executor.getQueuedTaskCount.toInt
}
