package flowlib

trait Monitored {
  def waiters: Int
  def backlog: Int
  def quota: Int
}

object Monitored {
  import Process.{continue, stop}
  import ProcessUtil.{forever, foreach, Sink}
  import Timing.repeatAfter

  case class Snapshot( label: String, stamp: Long, waiters: Int, backlog: Int, quota: Int) // ? extends Monitored

  def monitor(period: Long, lms: List[(String, Monitored)]): Sink[Snapshot] => Process[Nothing] = {
    output =>
      continue {
        val tick = repeatAfter( 0l, period)
        forever {
          tick >> {
            val ss =
              for((l, m) <- lms)
              yield Snapshot(l, System.currentTimeMillis, m.waiters, m.backlog, m.quota)

            foreach(ss)(output)
          }
        }
      }
  }
}
