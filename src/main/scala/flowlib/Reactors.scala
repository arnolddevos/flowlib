package flowlib

import Process._
import ProcessUtil._


/**
 * Provide a Reactor trait similar to the original scala Actor or Reactor.
 * However, methods yield with a Process instead of throwing an exception.
 */
trait Reactors {
  
  def backlog: Int
  def period: Long
  def site: Site

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
