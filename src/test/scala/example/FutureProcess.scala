
trait FutureProcess {
  import scalaz.concurrent.Future
  import Future.async
  import flowlib.Gate

  val input: Gate[String, String]
  val output: Gate[String, String]

  def headLines(n: Int): Future[Nothing] = 
    async(input.take) flatMap { line =>
      if(n == 0) headLines(0)
      else 
        async[Unit](k => output.offer(line)(k(()))) flatMap { _ =>
          headLines(n-1)
        }
    }
}
