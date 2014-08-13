package flowlib

import java.util.{Timer, TimerTask, Date}

trait Timing {
  object timer extends Timer(true)

  implicit class TimerThunk( k: => Unit ) extends TimerTask {
    def run = k
  }

  def idle[T]( delay: Long, t: T=()): Process[T] = {
    import Process._

    val slug = new Responder[T] {
      def respond( k: T => Unit ): Unit = timer.schedule(k(t), delay)
    }

    receive(slug)(stop(_))
  }

  def delayLine[T](delay: Long): Gate[T, T] = new Gate[T, T] {
    private val underlying = Gate.queue[T]()
    def accept(t: T): Unit = timer.schedule(underlying accept t, delay)
    def respond( k: T => Unit) = underlying respond k
  }

  def after[T]( delay: Long, t: T=()): Responder[T] = {
    val o = Gate.observable[T]()
    timer.schedule(o accept Some(t), delay)
    o
  }

  def at[T]( time: Long, t: T=()): Responder[T] = {
    val o = Gate.observable[T]()
    timer.schedule(o accept Some(t), new Date(time))
    o
  }
  def repeatAfter[T]( delay: Long, period: Long, t: T=()): Responder[T] = {
    val o = Gate.barrier()
    timer.schedule(o accept (()), delay, period)
    o map { _ => t }
  }

  def repeatAt[T]( time: Long, period: Long, t: T=()): Responder[T] = {
    val o = Gate.barrier()
    timer.scheduleAtFixedRate(o accept (()), new Date(time), period)
    o map { _ => t }
  }
}

object Timing extends Timing
