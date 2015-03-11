name := "flowlib"

organization := "au.com.langdale"

versionWithGit

git.baseVersion := "0.9"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.1.0" % "test"
