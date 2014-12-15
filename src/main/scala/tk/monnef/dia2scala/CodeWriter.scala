package tk.monnef.dia2scala

import java.io.File

import scala.util.control.NonFatal
import scalaz._
import Scalaz._
import Utils._

object CodeWriter {
  def writeTextFile(data: EmittedCode, path: String): \/[String, CodeWriterSuccess.type] = {
    try {
      new File(path).mkdirs()
      val inFileToClasses = data.classes.groupBy(_.inFile)
      for {(scFile, classes) <- inFileToClasses} {
        val c = classes.head
        val pckgPath = path + "/" + c.inPackage.split(".").mkString("/")
        new File(pckgPath).mkdirs()
        val outFile = new File(pckgPath + "/" + scFile)
        printToFile(outFile) { p =>
          if (c.inPackage.nonEmpty) p.println(s"package ${c.inPackage}\n")
          classes.map(_.code).mkString("\n\n") |> p.print
        }
      }
      CodeWriterSuccess.right
    } catch {
      case NonFatal(e) => e.getMessage.left
    }
  }
}

case object CodeWriterSuccess
