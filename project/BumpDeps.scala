import sbt._
import Keys._

object BumpDep extends Build {
  def bumpDeps = Command.command("bumpDeps") { state =>
    val updates =
      for{
        dep <- IO.listFiles(file("."), "*.sbt")
        ext <- {
          for {
            level <- 1 to 2
            prefix = "../"*level
            ext = file(s"$prefix${dep.base}/target/${dep.name}")
            if ext.exists
          }
          yield ext
        }.take(1)
      }
      yield (dep, ext)

    if(updates.nonEmpty) {
      for((dep, ext) <- updates) {
        println(s"dependency: $ext")
        IO.copyFile(ext, dep)
      }
      state.reload
    }
    else state
  }

  override lazy val settings = super.settings ++ Seq(
    commands ++= Seq(bumpDeps)
  )
}
