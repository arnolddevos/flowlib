
package flowlib

import annotation.tailrec
import util.control.NonFatal
import java.util.concurrent.{ExecutorService, ForkJoinPool}

trait Site {
  import Process._
  import Decoration._

  // hooks
  def started[U](p0: Process[U]): Unit
  def success[U](p0: Process[U], u: U): Unit
  def failure[U](p0: Process[U], w: Decoration, e: Throwable): Unit

  // async steps are run on this
  def executor: ExecutorService

  private def resume[V,U](p0: Process[V], p: Process[U], w: Decoration)(k: U => Unit): Unit = {

    def async[X](x: X)(k: X => Unit): Unit = {
      executor execute new Runnable {
        def run: Unit = {
          try { k(x) }
          catch { case NonFatal(e) => failure(p0, w, e) }
        }
      }
    }

    def push(p: Process[U]): Unit = bounce(p)

    @tailrec
    def bounce(p: Process[U]): Unit = {
      p match {

        // most popular cases it is supposed
        case WaitingAsync(respond)     => respond((async(_)(k)))
        case Complete(u)               => k(u)

        // flatMap with special cases to prevent stack overflow
        case Sequential(p1, step) =>
          p1 match {
            case Complete(x)      => bounce(step(x))
            case Ready(step1)     => bounce(Sequential(step1(), step))
            case Parallel(p2, p3) => run(p2); bounce(Sequential(p3, step))
            case Sequential(p2, step2) 
              => bounce(Sequential(p2, (x: Any) => Sequential(step2(x), step)))
            case _     
              => resume(p0, p1, w)(t => push(step(t)))
          }

        // the remaining cases
        case Ready(step)               => bounce(step())
        case Waiting(respond)          => respond(k)
        case Asynchronous(step)        => async(())(_ => push(step()))
        case Parallel(p1, p2)          => run(p1); bounce(p2)
        case Decorated(w @(Will(_)|Immortal|Mortal), p1)
                                       => resume(p0, p1, w)(k)
        case Decorated(_, p1)          => bounce(p1)
        case Failed(e)                 => failure(p0, w, e)
      }
    }

    bounce(p)
  }

  final def run(p: Process[Any]): Unit = p match {
    case Parallel(p1, p2) => run(p1); run(p2)
    case _ => 
      started(p)
      resume(p, p, mortal)(success(p, _))
  }
}

class DefaultSite extends Site with Monitored {
  import Decoration._

  val executor = new ForkJoinPool
  def started[U](p0: Process[U]): Unit = ()
  def success[U](p0: Process[U], u: U): Unit =
    println(s"Completed $p0 with: $u")
  def failure[U](p0: Process[U], w: Decoration, e: Throwable): Unit = {
    println(s"Failed $p0 with: ${formatException(e)}")
    w match {
      case Immortal => run(p0)
      case _ => executor.shutdown
    }
  }
  def formatException(e: Throwable) = {
    val c = e.getCause
    if(c != null) s"$e cause: $c" else e.toString
  }

  def backlog = executor.getActiveThreadCount
  def quota = executor.getParallelism
  def waiters = executor.getQueuedTaskCount.toInt
}
