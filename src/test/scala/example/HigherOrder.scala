trait HigherOrder {
  import flowlib._
  import Flow._
  import Gate._

  type Input[A] = Process[A]
  type Output[A] = A => Process[Unit]

  type Row
  type Sum
 
  def foldRows: Input[Row] => Output[Sum] => Process[Nothing]
  def generateRows: Output[Row] => Process[Nothing]

  val backlog: Int
  val rows = channel[Row](backlog)
  val sums = channel[Sum](backlog)

  val ensemble = {
    generateRows :-> rows &
    rows ->: foldRows :-> sums
  } 

  DefaultSite() run ensemble
}
