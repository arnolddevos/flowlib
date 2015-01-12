package flowlib

import annotation.tailrec
import util.control.NonFatal
import java.util.concurrent.Executor
import Gate.Latch

trait Site {
  import Process._

  // hooks
  def started[U](p0: Process[U]): Unit
  def success[U](p0: Process[U], u: U): Unit
  def failure[U](p0: Process[U], e: Throwable, r: Recovery): Unit

  // async steps are run on this
  def executor: Executor

  private def resume[V,U](p0: Process[V], p: Process[U], r: Recovery)(k: U => Unit): Unit = {

    def async[X](x: X)(k: X => Unit): Unit = {
      executor execute new Runnable {
        def run: Unit = {
          try { k(x) }
          catch { case NonFatal(e) => failure(p0, e, r) }
        }
      }
    }

    def alternatives(p1: Process[U], p2: Process[U]): Unit = {
      val l = Gate.latch[U]
      val s: U => Unit = u => l signal u
      resume(p0, p1, r)(s)
      resume(p0, p2, r)(s)
      l take k
    }

    def push(p: Process[U]): Unit = bounce(p)

    @tailrec
    def bounce(p: Process[U]): Unit = if(alive) {
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
              => resume(p0, p1, r)(t => push(step(t)))
          }

        // the remaining cases
        case Ready(step)               => bounce(step())
        case Waiting(respond)          => respond(k)
        case Asynchronous(step)        => async(())(_ => push(step()))
        case Parallel(p1, p2)          => run(p1); bounce(p2)
        case Alternative(p1, p2)       => alternatives(p1, p2)
        case Recoverable(p1, recovery) => resume(p0, p1, recovery)(k)
        case Named(p1, _)              => bounce(p1)
        case Failed(e)                 => failure(p0, e, r)
      }
    }

    bounce(p)
  }

  @volatile private var alive = true

  private def killer: Recovery = {
    (p0, e) =>
      alive = false
      stop(())
  }

  final def run(p: Process[Any]): Unit = p match {
    case Parallel(p1, p2) => run(p1); run(p2)
    case _ => 
      started(p)
      resume(p, p, killer)(success(p, _))
  }
}

object EventMachine extends Site {
  import Executors._
  import Process._

  val executor = trampoline(fifo)
  def started[U](p0: Process[U]): Unit = ()
  def success[U](p0: Process[U], u: U): Unit =
    println(s"Completed $p0 with: $u")
  def failure[U](p0: Process[U], e: Throwable, r: Recovery): Unit = {
    println(s"Failed $p0 with: ${formatException(e)}")
    run(s"Recovery of $p0" !: continue(r(p0, e)))
  }
  def formatException(e: Throwable) = {
    val c = e.getCause
    if(c != null) s"$e cause: $c" else e.toString
  }
}
