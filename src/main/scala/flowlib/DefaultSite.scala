package flowlib

import java.util.concurrent.ForkJoinPool

trait DefaultSite extends Site with Monitored

object DefaultSite {

  lazy val forkJoin = new ForkJoinPool

  def apply(printer: String => Unit = println _): DefaultSite = new DefaultSite with Site.Skeleton {
    def log(m: String) = printer(m)
    def executor = forkJoin
    def backlog = executor.getActiveThreadCount
    def quota = executor.getParallelism
    def waiters = executor.getQueuedTaskCount.toInt
  }
}
