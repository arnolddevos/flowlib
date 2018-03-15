name := "flowlib"

organization := "com.bgsig"

enablePlugins(GitVersioning)

unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "jvm" / "scala"

git.useGitDescribe := true
