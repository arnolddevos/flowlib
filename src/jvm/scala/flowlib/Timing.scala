package flowlib

import java.util.{Timer, TimerTask, Date}

object Timing {
  object timer extends Timer(true)

  implicit class TimerThunk( k: => Unit ) extends TimerTask {
    def run = k
  }

  /**
   * Each run, this process will wait for
   * delay ms and then complete.
   */
  def after( delay: Long): Process[Unit] = 
    Process.waitFor(k => timer.schedule(k(()), delay))


  /**
   * Each run, this process will wait until 
   * time ms after the epoch and then complete.
   */
  def at( time: Long): Process[Unit] = 
    Process.waitFor(k => timer.schedule(k(()), new Date(time)))


  /**
   * Each run, this process will wait until
   * the next 'tick' and then return the 'tick number'.
   * Ticks occur delay ms after repeatAfter is called and
   * every period ms after that.
   */
  def repeatAfter( delay: Long, period: Long): Process[Long] = {
    val b = Gate.barrier()
    timer.schedule(b signal (()), delay, period)
    Process.waitFor(b.take)
  }

  /**
   * Each run, this process will wait until
   * the next 'tick' and then return the 'tick number'.
   * Ticks occur at time ms after the epoch and
   * every period ms after that.
   */
  def repeatAt[T]( time: Long, period: Long): Process[Long] = {
    val b = Gate.barrier()
    timer.scheduleAtFixedRate(b signal (()), new Date(time), period)
    Process.waitFor(b.take)
  }
}
