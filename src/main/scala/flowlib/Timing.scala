package flowlib

import java.util.{Timer, TimerTask, Date}

object Timing {
  object timer extends Timer(true)

  implicit class TimerThunk( k: => Unit ) extends TimerTask {
    def run = k
  }

  def after( delay: Long): Process[Unit] = 
    Process.waitFor(k => timer.schedule(k(()), delay))

  def at( time: Long): Process[Unit] = 
    Process.waitFor(k => timer.schedule(k(()), new Date(time)))

  trait Ticker {
    def nextTick: Process[Long]
  }

  def repeatAfter( delay: Long, period: Long) = new Ticker {
    private val b = Gate.barrier()
    timer.schedule(b signal (()), delay, period)
    def nextTick = Process.waitFor(b.take)
  }

  def repeatAt[T]( time: Long, period: Long) = new Ticker {
    private val b = Gate.barrier()
    timer.scheduleAtFixedRate(b signal (()), new Date(time), period)
    def nextTick = Process.waitFor(b.take)
  }
}
