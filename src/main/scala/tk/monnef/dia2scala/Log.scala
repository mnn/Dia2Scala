package tk.monnef.dia2scala

object Log {

  var currentVerbosityLevel: VerbosityLevel = NotVerbose

  private def printIfLowerLevel(msg: String, level: VerbosityLevel) {
    if (level.level <= currentVerbosityLevel.level) println(msg)
  }

  val printCritical = printIfLowerLevel(_: String, Quiet)
  val printInfo = printIfLowerLevel(_: String, NotVerbose)
  val printDebug = printIfLowerLevel(_: String, Verbose)
  val printTrace = printIfLowerLevel(_: String, ExtraVerbose)
}

sealed class VerbosityLevel(val level: Int)

case object Quiet extends VerbosityLevel(0)

case object NotVerbose extends VerbosityLevel(1)

case object Verbose extends VerbosityLevel(2)

case object ExtraVerbose extends VerbosityLevel(3)
