package flowlib
package tests

import org.scalatest._

abstract class Testing extends FlatSpec with Matchers with concurrent.AsyncAssertions {
  import java.util.concurrent.atomic.AtomicInteger
  import java.util.concurrent.ThreadLocalRandom

  def transCannons(cannons: Int, damageLimit: Int, shotLimit: Int)( target: (Int, Waiter, AtomicInteger) => Unit): (Int, Int) = {
    val damage = new AtomicInteger(0)
    val shots  = new AtomicInteger(0)
    val w = new Waiter

    val battery = for( c <- 0 until cannons ) yield {
      new Thread {
        override def run: Unit = {
          while( damage.get < damageLimit && shots.get < shotLimit ) {
            val s = shots.getAndIncrement
            val i = ThreadLocalRandom.current.nextInt(0, damageLimit)
            // println(s"> $s $i ${damage.get} ${shots.get}")
            target(i, w, damage)
            // println(s"< $s $i ${damage.get} ${shots.get}")
          }
          w.dismiss
        }
      }
    }

    for( c <- battery ) c.start
    w.await(dismissals(cannons))

    (damage.get, shots.get)
  }

  def transSeq(cannons: Int, seqLength: Int)( target: (Int, Int, Waiter) => Unit): Unit = {
    val damage = new AtomicInteger(0)
    val shots  = new AtomicInteger(0)
    val w = new Waiter

    val battery = for( c <- 0 until cannons ) yield {
      new Thread {
        override def run: Unit = {
          for( i <- 0 until seqLength ) {
            target(c, i, w)
          }
          w.dismiss
        }
      }
    }

    for( c <- battery ) c.start
    w.await(dismissals(cannons))
  }
}
