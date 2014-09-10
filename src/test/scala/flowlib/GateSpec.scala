package flowlib
package tests

import org.scalatest._

class GateSpec extends Testing {
  "a channel" should "yield values FIFO order" in {
    val w = new Waiter
    val c = Gate.channel[Int](10)
    val K1 = 42; val K2 = 87
    c.offer(K1) { w.dismiss }
    c.take { i => w { i shouldBe K1 }; w.dismiss }
    c.take { i => w { i shouldBe K2 }; w.dismiss }
    c.offer(K2) { w.dismiss }
    w.await(dismissals(4))
  }

  it should "accept values from many threads" in {
    val N = 5; val M = 100
    val c = Gate.channel[(Int, Int)](10)
    transSeq(N, 100) { (cannon, value, w) =>
      c.signal((cannon, value))
    }

    val w = new Waiter
    val v = Array.fill(N)(-1)

    new Thread {
      override def run: Unit = {
        val s = new scala.concurrent.SyncVar[Unit]
        for( n <- 0 until N * M) {
          c take { 
            case (cannon, value) =>
              w { 
                value shouldBe v(cannon) + 1 
                v(cannon) = value
                s.put(())
              }
          }
          s.take
        }
        w.dismiss
      }
    }.start

    w.await
    // println(v.mkString(","))
  }

  it should "block when full" in {
    val N = 3
    val w = new Waiter
    val c = Gate.channel[Unit](N)
    var n = 0
    for( i <- 0 until N * 2)
      c.offer(()){ n += 1 }
    n shouldBe N
  }
}
