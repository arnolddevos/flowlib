package flowlib

import annotation.tailrec
import util.control.NonFatal
import java.util.concurrent.{ExecutorService, Executor, ForkJoinPool}
import Gate.Latch

class DefaultSite extends Site with Monitored {
  import Decoration._

  val executor = new ForkJoinPool
  def latch[U] = Gate.latch[U]
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
