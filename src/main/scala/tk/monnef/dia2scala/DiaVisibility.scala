package tk.monnef.dia2scala

object DiaVisibility extends Enumeration {
  type DiaVisibility = Value
  val Public, Private, Protected, Implementation = Value

  final val dvToCode = Map(
    Public -> "public",
    Private -> "private",
    Protected -> "protected",
    Implementation -> "???"
  )

  implicit class DiaVisibilityPimps(val dv: DiaVisibility) {
    def code: String = dvToCode(dv)
  }

}
