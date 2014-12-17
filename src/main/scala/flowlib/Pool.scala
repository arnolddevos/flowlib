package flowlib

trait Pool[Resource] {
  import Pool._
  def submitOnce[T:Errors]( job: Resource => T ): Process[T]
  def submitAndRetry[T:Errors]( job: Resource => T ): Process[T]
}

object Pool {

  trait LifeCycle[Resource] {
    def create: Process[Resource]
    def destroy(r: Resource): Process[Unit]
    def check(r: Resource): Process[Boolean]
    def aged: Int
  }

  trait Errors[T] {
    type Result = T
    def isError(t: Result): Boolean
    def indicateError(cause: String): Result
  }

  object Errors {
    implicit def forEither[T] = new Errors[Either[Throwable, T]] {
      def isError(r: Result) = r match {
        case Left(t) => true
        case Right(t) => false
      }
      def indicateError(cause: String): Result = Left(new RuntimeException(cause))
    }
  }

  def create[R](size: Int)(implicit cycle: LifeCycle[R]): Pool[R] = new Pool[R] {
    def submitOnce[T:Errors]( job: R => T ) = ???
    def submitAndRetry[T:Errors]( job: R => T ) = ???
  }
}
