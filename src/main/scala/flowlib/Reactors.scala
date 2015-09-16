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

  trait Reactor[A] {
    val mailbox = new TimedQueue[A](backlog, period)
    
    def !( a: A): Unit = 
      mailbox signal a
    
    def !!( a: A): Process[Unit] = 
      waitDone(mailbox offer a)
    
    def react( f: A => Process[Unit]) = 
      waitFor(mailbox.take) >>= f
    
    def reactWithin(ms: Long)( f: Option[A] => Process[Unit]) = 
      waitFor(mailbox.takeWithin(ms)) >>= f
    
    def start = site run process(act)

    def act: Process[Unit]
  }
}
