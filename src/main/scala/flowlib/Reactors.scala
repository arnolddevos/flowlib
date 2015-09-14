package flowlib

import Process._
import ProcessUtil._

trait Reactors {
  
  def backlog: Int
  def period: Long
  def site: Site

  trait Reactor[A] {
    private val mailbox = new TimedQueue[A](backlog, period)
    
    def !( a: A): Process[Unit] = 
      waitDone(mailbox offer a)
    
    def react( f: A => Process[Unit]) = 
      waitFor(mailbox.take) >>= (s => f(s.get))
    
    def reactWithin(ms: Long)( f: Option[A] => Process[Unit]) = 
      waitFor(mailbox.takeWithin(ms)) >>= f
    
    def act: Process[Unit]
    def start = site run process(act)
  }
}
