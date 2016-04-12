import sbt._
import Keys._

object BumpDep extends Build {

  def bumpDeps = Command.command("bumpDeps") { state =>
    for(dep <- IO.listFiles(file("."), "*.sbt")) { 
      val ext = file(s"../${dep.base}/target/${dep.name}")
      if( ext.exists) {
        println(s"dependency: $ext")
        IO.copyFile(ext, dep)
      }
    }
    state.reload
  }



  override lazy val settings = super.settings ++ Seq(
    commands ++= Seq(bumpDeps)
  )
}
