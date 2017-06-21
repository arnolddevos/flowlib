package flowlib

import java.util.concurrent.ForkJoinPool
import Site._

trait DefaultSite extends Site with Monitored with SiteFailFast with SiteLogAll

object DefaultSite {

  lazy val forkJoin = new ForkJoinPool

  def apply(printer: String => Unit = println _, hook: Throwable => Unit = _ => ()): DefaultSite = new DefaultSite {
    def log(m: String) = printer(m)
    def shutdown(e: Throwable) = hook(e)
    def executor = forkJoin
    def backlog = executor.getActiveThreadCount
    def quota = executor.getParallelism
    def waiters = executor.getQueuedTaskCount.toInt
  }
}
