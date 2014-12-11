name := "Dia2Scala"

version := "0.1"

scalaVersion := "2.11.4"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)

buildInfoPackage := "tk.monnef.dia2scala"

// disable using the Scala version in output paths and artifacts
crossPaths := false

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  "dia2scala" + "_" + module.revision + "." + artifact.extension
}
