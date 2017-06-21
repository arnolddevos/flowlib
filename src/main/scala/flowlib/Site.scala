package flowlib

import annotation.tailrec
import util.control.NonFatal
import java.util.concurrent.Executor
import Executors._
import Gate.Latch
import Process._

trait Site {

  // hooks
  def started[U](p0: Process[U]): Unit
  def success[U](p0: Process[U], u: U): Unit
  def failure[U](p0: Process[U], e: Throwable, r: Recovery): Unit
  def poisoned: Boolean
  def defaultRecovery: Recovery

  // async steps are run on this
  def executor: Executor

  private def resume[V,U](p0: Process[V], p: Process[U], r: Recovery)(k: U => Unit): Unit = {

    def async[X](x: X)(k: X => Unit): Unit = {
      if(! poisoned) {
        executor execute new Runnable {
          def run: Unit = {
            try { k(x) }
            catch { case NonFatal(e) => failure(p0, e, r) }
          }
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

  final def run(p: Process[Any]): Unit = p match {
    case Parallel(p1, p2) => run(p1); run(p2)
    case _ =>
      started(p)
      resume(p, p, defaultRecovery)(success(p, _))
  }
}

object Site {

  trait SiteLogAll { this: Site =>

    def log(m: String): Unit

    def started[U](p0: Process[U]): Unit = ()
    def success[U](p0: Process[U], u: U): Unit =
      log(s"Completed $p0 with: $u")
    def failure[U](p0: Process[U], e: Throwable, r: Recovery): Unit = {
      log(s"Failed $p0 with: ${formatException(e)}")
      run(s"Recovery of $p0" !: continue(r(p0, e)))
    }
  }

  trait SiteQuiet  { this: Site =>

    def started[U](p0: Process[U]): Unit = ()
    def success[U](p0: Process[U], u: U): Unit = ()
    def failure[U](p0: Process[U], e: Throwable, r: Recovery): Unit = {
      run(s"Recovery of $p0" !: continue(r(p0, e)))
    }
  }

  trait SiteFailFast { this: Site =>

    def shutdown(e: Throwable): Unit

    @volatile private var poisonV = false
    final def poisoned = poisonV

    final def poison(e: Throwable) = {
      poisonV = true
      shutdown(e)
    }

    val defaultRecovery: Recovery = {
      (p0, e) =>
        poison(e)
        stop(())
    }
  }

  trait SiteNoFail { this: Site =>
    final def poisoned = false
    val defaultRecovery: Recovery = (_, _) => stop(())
  }

  def formatException(e: Throwable) = {
    val c = e.getCause
    if(c != null) s"$e cause: $c" else e.toString
  }

  trait EventMachine extends Site with SiteLogAll with SiteFailFast

  def eventMachine(printer: String => Unit = println _, hook: Throwable => Unit = _ => ()): Site = new EventMachine {
    def executor = trampoline(fifo)
    def log(m: String) = printer(m)
    def shutdown(e: Throwable) = hook(e)
  }
}
