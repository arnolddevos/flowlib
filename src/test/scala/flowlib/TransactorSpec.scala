package flowlib
package tests

import org.scalatest._

abstract class Testing extends FlatSpec with Matchers with concurrent.AsyncAssertions // with concurrent.Conductors 

class TransactorSpec extends Testing {
  import Transaction._

  "a transactor" should "execute the effect of an eligible transaction" in {
    val w = new Waiter
    val t = Transactor(0l)
    t.transact { case 0l => 1l } { x => w { x shouldBe 0l }; w.dismiss }
    w.await
  }

  it should "execute the state transition of an eligible transaction" in {
    val w = new Waiter
    val t = Transactor(0l)
    t.transact { case 0l => 1l } { noEffect }
    t.transact { noChange } { x => w { x shouldBe 1l }; w.dismiss }
    w.await
  }

  it should "not execute an ineligible transaction" in {
    val w = new Waiter
    val t = Transactor(0l)
    t.transact { case 1l => 2l } { x => fail }
    t.transact { noChange } { x => w { x shouldBe 0l }; w.dismiss }
    w.await
  }

  it should "retry a transaction after a state change" in {
    val w = new Waiter
    val t = Transactor(0l)
    t.transact { case 1l => 2l } { x => w { x shouldBe 1l }; w.dismiss }
    t.transact { case 0l => 1l } { x => w { x shouldBe 0l }; w.dismiss }
    w.await(dismissals(2))
  }

  it should "retry only one eligible transaction after a state change" in {
    val w = new Waiter
    val t = Transactor(0l)
    t.transact { case 1l => 2l } { x => w { x shouldBe 1l }; w.dismiss }
    t.transact { case 1l => 3l } { x => fail }
    t.transact { case 0l => 1l } { x => w { x shouldBe 0l }; w.dismiss }
    t.transact { noChange } { x => w { x shouldBe 2l }; w.dismiss }
    w.await(dismissals(3))
  }

  it should "sequence concurrent transactions" in {
    val limit = 200
    val t = Transactor(0)
    val (damage, shots) = transCannons(5, limit, limit*20) { (i, w, d) => 
      t.transact { case `i` => i + 1 } { x => val y = d.getAndIncrement; /* println(s"* $x $y") */ }
    }
    damage shouldBe limit
    shots should be >= damage
    println(s"damage/shots=$damage/$shots")
  }

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
}
