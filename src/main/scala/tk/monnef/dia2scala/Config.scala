package tk.monnef.dia2scala

import java.io.File

import tk.monnef.dia2scala.BuildInfoWrapper._

case class Config(file: File = null, unpack: Boolean = true, verbosity: VerbosityLevel = NotVerbose, outputPath: String = "out", groupByDependency: Boolean = false, useDefaultImportTable: Boolean = true)

object CommandLineParser {
  def apply(args: Array[String]): Option[Config] = {
    val parser = new scopt.OptionParser[Config]("dia2scala") {
      head(title, BuildInfoWrapper.version, "\n", createdBy, "\n", s"Compiled using SBT $sbtVersion and Scala $scalaVersion.")
      opt[File]('f', "file") required() valueName "<file>" action { (x, c) =>
        c.copy(file = x)
      } text "input dia file"
      opt[Unit]('u', "unpacked") action { (_, c) =>
        c.copy(unpack = false)
      } text "skips unpacking of an input dia file"
      help("help") text "prints this usage text"
      opt[Unit]('q', "quiet") action { (_, c) =>
        c.copy(verbosity = Quiet)
      } text "suppresses all non critical output"
      opt[Unit]('v', "verbose") action { (_, c) =>
        c.copy(verbosity = Verbose)
      } text "prints extra debug information"
      opt[Unit]("veryverbose") abbr "vv" action { (_, c) =>
        c.copy(verbosity = ExtraVerbose)
      } text "prints a lot of debug information"
      opt[String]('o', "outputpath") valueName "<path>" action { (x, c) =>
        c.copy(outputPath = x)
      } text "output directory"
      opt[Unit]('d', "groupbydependency") action { (_, c) =>
        c.copy(groupByDependency = true)
      } text "tries group related classes to one source file"
      opt[Unit]("suppress-default-import-table") action { (_, c) =>
        c.copy(useDefaultImportTable = false)
      } text "forbids usage of default import table containing several Scala classes"
    }
    parser.parse(args, Config())
  }
}
