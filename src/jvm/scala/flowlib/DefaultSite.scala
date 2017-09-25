package flowlib

import java.util.concurrent.ForkJoinPool
import Site._
import DefaultSite._

trait DefaultSite extends Site with SiteForkJoin with SiteFailFast with SiteLogAll

object DefaultSite {

  lazy val forkJoin = new ForkJoinPool

  trait SiteForkJoin extends Monitored { this: Site with SiteForkJoin =>
    def executor: ForkJoinPool
    def backlog = executor.getActiveThreadCount
    def quota = executor.getParallelism
    def waiters = executor.getQueuedTaskCount.toInt
  }

  def apply(printer: String => Unit = println _, hook: Throwable => Unit = _ => ()): DefaultSite = new DefaultSite {
    def executor = forkJoin
    def log(m: String) = printer(m)
    def shutdown(e: Throwable) = hook(e)
  }
}
