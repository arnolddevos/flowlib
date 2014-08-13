
package flowlib

import annotation.tailrec
import util.control.NonFatal
import java.util.concurrent.{ExecutorService, ForkJoinPool}
import java.util.{NoSuchElementException => NSE}
import Labels._

trait Site {
  import Process._

  def labels: NaturalMap[Label, Responder]
  def success[U](p0: Process[U], u: U): Unit
  def failure[U](p0: Process[U], e: Throwable): Unit
  def executor: ExecutorService

  def ready[U](p0: Process[U]): Responder[U] = new Responder[U] {
    def respond(k: U => Unit): Unit = {
      @tailrec
      def bounce(p: Process[U]): Unit = {

        def waiting[T](gate: Responder[T], step: T => Process[U]): Unit =
          gate respond { t => bounceBack(step(t)) }

        def async(step: () => Process[U]): Unit =
          executor execute new Runnable {
            def run: Unit = bounceBack(evaluate(step))
          }

        def evaluate(step: () => Process[U]): Process[U] =
          try { step() }
          catch { case NonFatal(e) => Failed(e) }

        p match {
          case Ready(step)          => bounce(evaluate(step))
          case Waiting(gate, step)  => waiting( gate, step )
          case Asynchronous(step)   => async(step)
          case Sequential(p1, step) => waiting(ready(p1), step)
          case Parallel(p1, step)   => run(p1); bounce(step)
          case Complete(u)          => k(u)
          case Named(_, p1)         => bounce(p1)
          case Labelled(label, step)=>
            labels get label match {
              case Some(gate) => waiting(gate, step)
              case _ => bounce(Failed(new NSE(label.toString)))
            }
          case Failed(e)            => failure(p0, e)
        }
      }
      def bounceBack(p: Process[U]): Unit = bounce(p)
      bounce(p0)
    }
  }

  def run[U](p0: Process[U]): Unit = ready(p0) respond (success(p0, _))
}

trait DefaultSite extends Site {
  def labels=NaturalMap.empty[Label, Responder]
  def executor: ExecutorService = new ForkJoinPool
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
}

object Site extends DefaultSite
