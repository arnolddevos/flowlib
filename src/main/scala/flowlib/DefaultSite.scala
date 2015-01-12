package flowlib

import annotation.tailrec
import util.control.NonFatal
import java.util.concurrent.{ExecutorService, Executor, ForkJoinPool}
import Gate.Latch

class DefaultSite extends Site with Monitored {
  import Process._

  val executor = DefaultSite.executor
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

  def backlog = executor.getActiveThreadCount
  def quota = executor.getParallelism
  def waiters = executor.getQueuedTaskCount.toInt
}

object DefaultSite {
  def apply() =new DefaultSite
  val executor = new ForkJoinPool
}
