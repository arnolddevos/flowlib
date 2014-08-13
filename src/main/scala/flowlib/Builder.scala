package flowlib
import Labels._
import Process._

import java.util.concurrent.{ExecutorService, ForkJoinPool}
import scala.language.existentials
import scala.collection.immutable.BitSet

/**
 * Given a graph of Processes construct an execution object for each
 * and channels to link them.
 *
 * The input and output operations create process steps (of type Labelled)
 * that will dispatch messages according to labels in the process graph.
 *
 * The run method creates the channels and labels them and runs each process
 * in this context, called a site.
 *
 * Arbitrary graph topology is allowed.  A single shared channel with
 * backpressure is provided for one to one, fan-in and fan-out topologies.
 *
 * In general, this is the implementation for any fully meshed group of
 * producers and consumers where any consumer can handle any message.
 *
 * When a group is not fully meshed a channel with tagged messages is
 * provided. The tags are arranged to agree with the topology while
 * a common pool of messages ensures none are stranded (as in a network
 * of channels).
 *
 * See also Wiring for a simpler if less general approach.
 *
 */
trait Builder extends Graphs {

  type Node = Process[Any]
  case class FlowOut[T](label: LabelOut[T]) extends Label[Acceptor[T]]
  case class FlowIn[T](label: LabelIn[T]) extends Label[T]
  case class BoundFlow(node: Node, label: Label[_])
  case class Arc(out: BoundFlow, in: BoundFlow)

  def arc[Message](node1: Node, port1: LabelOut[Message], port2: LabelIn[Message], node2: Node) =
    Arc(BoundFlow(node1, FlowOut(port1)), BoundFlow(node2, FlowIn(port2)))

  val channelDepth = 3

  def run(graph: Graph): Map[Node, Site] = {

    val djs = {
      val djs0 = DisjointSets.empty[BoundFlow]
      graph.arcs.foldLeft(djs0) { (djs, a) => djs.union(a.out, a.in) }
    }

    val i2os: Map[BoundFlow,Set[BoundFlow]] =
                graph.arcs groupBy (_.in) mapValues (_ map (_.out))

    def iorss =
      for( dj <- djs.sets.values )
      yield {
        val (is, os) = dj partition (i2os contains _)
        val meshed = is forall { i => i2os(i) == os }
        if(meshed) {
          val c = new Channel[Any](os.size * channelDepth)
          (for( i <- is) yield i -> c.output) ++
          (for( o <- os) yield o -> c.input)
        }
        else {
          val c = new Switch[Int, Any](os.size * channelDepth)
          val o2k = os.zipWithIndex.toMap
          val i2ks =
            for(i <- is)
            yield {
              val ks = for(o <- i2os(i)) yield o2k(o)
              i -> (BitSet.empty ++ ks)
            }
          (for((o, k) <- o2k) yield o -> c.input(k)) ++
          (for((i, ks) <- i2ks) yield i -> c.output(ks contains _))
        }
      }

    val fjp = new ForkJoinPool

    val pss =
      for((p, iors) <- iorss.flatten groupBy (_._1.node))
      yield {

        val lrs = for((BoundFlow(p, l), r) <- iors) yield l -> r
        val l2r = NaturalMap.unsafe[Label, Responder](lrs.toMap)

        p -> new DefaultSite {
          override def labels=l2r
          override def executor: ExecutorService=fjp
        }
      }

    for((p, s) <- pss)
      s run p

    pss.toMap
  }

  def input[T, U]( port: LabelIn[T])( step: T => Process[U]): Process[U] =
    Labelled(FlowIn[T](port), step)

  def output[T, U]( port: LabelOut[T], t: T)( step: => Process[U]): Process[U] =
    Labelled(FlowOut[T](port), {o: Acceptor[T] => o accept t; step})
}
