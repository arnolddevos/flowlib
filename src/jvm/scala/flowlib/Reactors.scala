package flowlib

import flowlib.{Process=>P, Timing=>T}
import Process.{waitDone, waitFor}


/**
 * Provide a Reactor trait similar to the original scala Actor or Reactor.
 * However, methods yield with a Process instead of throwing an exception.
 */
trait Reactors {
  
  def backlog: Int
  def period: Long
  def site: Site
  
  type Actor = Reactor[Any]
  type Process[+U] = P[U]
  def stop[U](u: U): Process[U] = P.stop(u)
  def process[U](u: => Process[U]): Process[U] = P.process(u)
  def after(ms: Long): Process[Unit] = T.after(ms)

  case object PoisonPill 

  trait OutputChannel[-A] {
    def !( a: A): Unit
    def !!( a: A): Process[Unit]
  }

  trait OutputProxy[-A] extends OutputChannel[A] {
    def !(a: A) = self ! a
    def !!(a: A) = self !! a
    protected def self: OutputChannel[A]
  }

  trait Reactor[A] extends OutputChannel[A] {
    private val mailbox = new TimedQueue[A](backlog, period)
    
    final def !( a: A): Unit = 
      mailbox signal a
    
    final def !!( a: A): Process[Unit] = 
      waitDone(mailbox offer a)
    
    final protected def react( f: A => Process[Unit]) = 
      waitFor(mailbox.take) >>= f
    
    final protected def reactWithin(ms: Long)( f: Option[A] => Process[Unit]) = 
      waitFor(mailbox.takeWithin(ms)) >>= f
    
    final def start = site run process(act)

    protected def act: Process[Unit]
  }
}

object Reactors extends Reactors {
  def backlog = 10
  def period  = 10l
  def site    = DefaultSite()
}
