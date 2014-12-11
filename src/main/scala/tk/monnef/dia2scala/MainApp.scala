package tk.monnef.dia2scala

import Log.{printCritical, printDebug, printInfo, printTrace}

object MainApp extends App {
  CommandLineParser(args).map { config =>
    Log.currentVerbosityLevel = config.verbosity
    printTrace(s"Config: $config")
  } getOrElse {
    System.err.println("Error in command line arguments.")
  }
}
