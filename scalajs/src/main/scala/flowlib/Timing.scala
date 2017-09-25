package flowlib

import org.scalajs.dom.window.{setInterval, setTimeout, clearInterval, clearTimeout}

object Timing {
  /**
   * Each run, this process will wait for
   * delay ms and then complete.
   */
  def after( delay: Double): Process[Unit] = 
    Process.waitFor(k => setTimeout(() => k(()), delay))

  /**
   * Each run, this process will wait until
   * the next 'tick' and then return the 'tick number'.
   * Ticks occur delay ms after repeatAfter is called and
   * every period ms after that.
   */
  def repeatAfter( delay: Double, period: Double): Process[Long] = {
    require(delay == 0.0)
    val b = Gate.barrier()
    setInterval(() => b signal (()), period)
    Process.waitFor(b.take)
  }
}
