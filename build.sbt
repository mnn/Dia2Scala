name := "Dia2Scala"

version := "0.1"

scalaVersion := "2.12.0"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.7"

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)

buildInfoPackage := "tk.monnef.dia2scala"

// disable using the Scala version in output paths and artifacts
crossPaths := false

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  "dia2scala" + "_" + module.revision + "_slim" + "." + artifact.extension
}

assemblyJarName in assembly := s"${name.value.toLowerCase}_${version.value}.jar"

