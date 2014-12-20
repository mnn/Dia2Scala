package tk.monnef.dia2scala

import scalaz._
import Scalaz._

import Log.{printCritical, printDebug, printInfo, printTrace}

object MainApp extends App {

  def main(): Int = {
    CommandLineParser(args).map { config =>
      Log.currentVerbosityLevel = config.verbosity
      printTrace(s"Config: $config")
      process(config)
    } getOrElse {
      System.err.println("Error in command line arguments.")
      1
    }
  }

  private def process(config: Config): Int =
    (for {
      parsedFile <- XmlParser.parseFile(config.file, config.unpack)
      code <- CodeEmitter.emit(parsedFile)
      result <- CodeWriter.writeTextFile(code, config.outputPath, config.groupByDependency)
    } yield result).
      fold(
        err => {printCritical(err); 2},
        _ => 0)

  System.exit(main())
}
