package flowlib

sealed trait Decoration

object Decoration {
  case class Name( val text: String ) extends Decoration
  case object Immortal extends Decoration
  case object Mortal extends Decoration
  case class Will(x: Any) extends Decoration
  val immortal: Decoration = Immortal
} 

trait IsDecor[T] {
  def prove(t: T): Decoration
}

object IsDecor {
  implicit val nameIsEtiquette = new IsDecor[String] {
    def prove( name: String ) = Decoration.Name(name)
  }
  implicit val itIsEtiquette = new IsDecor[Decoration] {
    def prove( d: Decoration) = d
  }
}
