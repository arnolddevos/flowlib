name := "flowlib"

organization := "com.bgsig"

version := "0.10"

unmanagedSourceDirectories in Compile += baseDirectory.value.getParentFile / "src" / "main" / "scala"

enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

scalaVersion := "2.12.2" 

scalacOptions += "-P:scalajs:sjsDefinedByDefault"
