publishLocal := { 
  def depends = 
    s""""${organization.value}" %% "${name.value}" % "${version.value}""""
  IO.write(file(s"${target.value}/${name.value}.sbt"), s"libraryDependencies += $depends\n")
  publishLocal.value 
}
