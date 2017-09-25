name := "flowlib"

organization := "com.bgsig"

enablePlugins(DottedGitVersioning)

unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "jvm" / "scala"
