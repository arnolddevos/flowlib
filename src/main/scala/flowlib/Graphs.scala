package flowlib

import scala.language.higherKinds
import Labels._

/**
 * Defines a representation for a directed labelled graph
 * in which the arc labels carry a type parameter.
 *
 * The actual types for Node, Arc and Label are deferred.
 * There are two labels per arc called the output and input port label
 * respectively.  The type of the latter must encompass that of the former.
 *
 * The Graph type includes two sets of boundary nodes, heads and tails,
 * used when defining arcs between two sub-graphs.
 */
trait Graphs {

  type Node
  type Arc

  def arc[Message](node1: Node, port1: LabelOut[Message], port2: LabelIn[Message], node2: Node): Arc

  case class Graph( arcs: Set[Arc], heads: Set[Node], tails: Set[Node])
  case class LeftProject( arms: List[(Graph, Wiring)] )
  case class RightProject( arms: List[(Wiring, Graph)] )

  sealed trait Wiring
  sealed trait SingleWiring extends Wiring
  case class Direct[Message](label: Label[Message] ) extends SingleWiring
  case class Splice[Message]( output: LabelOut[Message], input: LabelIn[Message]) extends SingleWiring
  case class Bundle( wirings: Set[SingleWiring]) extends Wiring

  def decompose( wiring: Wiring): Set[SingleWiring] = wiring match {
    case Bundle( wirings ) => wirings
    case single: SingleWiring => Set(single)
  }

  def merge(graph: Graph, other: Graph): Graph = { import graph._
    Graph( arcs ++ other.arcs, heads ++ other.heads, tails ++ other.tails )
  }

  def connect(graph: Graph, wiring: Wiring, target: Graph): Graph = {
    val arcs = for {
      single <- decompose(wiring)
      node1 <- graph.tails
      node2 <- target.heads
    }
    yield single match {
      case Direct( label ) => arc(node1, label, label, node2)
      case Splice( output, input ) => arc(node1, output, input, node2)
    }
    Graph( graph.arcs ++ target.arcs ++ arcs, graph.heads, target.tails)
  }

  def connectLeft( project: LeftProject, target: Graph): Graph = {
    val graphs =
      for { (graph, wiring) <- project.arms }
        yield connect(graph, wiring, target)
    graphs.reduce(merge)
  }

  def connectRight( graph: Graph, project: RightProject): Graph = {
    val graphs =
      for { (wiring, target) <- project.arms }
        yield connect(graph, wiring, target)
    graphs.reduce(merge)
  }
}

/**
 * A graph construction DSL.
 */
trait GraphDSL extends Graphs {

  implicit class GraphOps( val graph: Graph) {
    def :-( rhs: WiringOps) = LeftProject(List((graph, rhs.wiring)))
    def ->:( lhs: WiringOps ) =  RightProject( List((lhs.wiring, graph)) )
    def &( other: GraphOps): Graph = merge(graph, other.graph)
  }

  implicit class NodeOps( node: Node )
    extends GraphOps(Graph( Set(), Set(node), Set(node)))

  implicit class LeftProjectOps( project: LeftProject) {
    def :->(rhs: GraphOps): Graph = connectLeft(project, rhs.graph)
    def &( other: LeftProject) = LeftProject( project.arms ++ other.arms )
  }

  implicit class RightProjectOps( project: RightProject) {
    def -:( lhs: GraphOps) = connectRight(lhs.graph, project)
    def &( other: RightProject) = RightProject( project.arms ++ other.arms )
  }

  implicit class WiringOps( val wiring: Wiring) {
    def +( other: WiringOps ) = Bundle( decompose(wiring) ++ decompose(other.wiring) )
  }

  implicit class LabelOps[Message](label: Label[Message])
    extends WiringOps(Direct(label))

  implicit class OutputOps[Message](output: LabelOut[Message]) {
    def /( input: LabelIn[Message]) = Splice( output, input)
  }
}
