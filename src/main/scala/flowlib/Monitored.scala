package flowlib

trait Monitored {
  def waiters: Int
  def backlog: Int
  def quota: Int
}

case class Snapshot( label: String, waiters: Int, backlog: Int, quota: Int) // ? extends Monitored

object Monitored {
  import Process.{continue, stop}
  import ProcessUtil.forever
  import Timing.repeatAfter
  import Wiring.Sink

  def monitor(period: Long, lms: List[(String, Monitored)]): Sink[List[Snapshot]] => Process[Nothing] = {
    output =>
      continue(stop(repeatAfter( 0l, period))) >>= {
        tick => 
          forever {
            tick >> output {
              for((l, m) <- lms)
              yield Snapshot(l, m.waiters, m.backlog, m.quota)
            }
          }
      }
  }
}
