package tk.monnef.dia2scala

object BuildInfoWrapper {
  lazy val buildInfoInstance = Class.forName("tk.monnef.dia2scala.BuildInfo")
  lazy val version = getAndInvokeMethod[String]("version")
  lazy val title = getAndInvokeMethod[String]("name")
  val name = "dia2scala"
  lazy val scalaVersion = getAndInvokeMethod[String]("scalaVersion")
  lazy val sbtVersion = getAndInvokeMethod[String]("sbtVersion")
  val createdBy = "Created by monnef"

  def getAndInvokeMethod[T](name: String): T = buildInfoInstance.getMethod(name).invoke(buildInfoInstance).asInstanceOf[T]
}
