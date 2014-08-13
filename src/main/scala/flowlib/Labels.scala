package flowlib

import scala.language.higherKinds

/**
 * Labels are used to identify an input or output port
 * at an execution Site, on a Process or in a graph.
 * 
 * When marked implicit, a label identifies a parameter
 * or result of a function lifted into a process.
 *
 * Each label is distinct and carries a type parameter.
 */

object Labels {

  trait LabelOut[+Message]
  trait LabelIn[-Message]
  trait Label[Message] extends LabelOut[Message] with LabelIn[Message]

  def label[Message] = new Label[Message] {}
  def label[Message]( descr: String ) = new Label[Message] { override def toString = descr }
}
