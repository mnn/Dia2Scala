package tk.monnef.dia2scala

import java.io.File

import scala.util.control.NonFatal
import scalaz._
import Scalaz._
import Utils._

object CodeWriter {
  def writeTextFile(data: EmittedCode, path: String, groupByDependency: Boolean): \/[String, CodeWriterSuccess.type] = {
    try {
      new File(path).mkdirs()

      val inFileToClasses =
        if (!groupByDependency) data.parts.groupBy(_.inFile)
        else groupAndSortByInheritance(data)

      for {(scFile, classes) <- inFileToClasses} {
        val c = classes.head
        val pckgPath = path + "/" + c.inPackage.split("\\.").mkString("/")
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

  def groupAndSortByInheritance(data: EmittedCode): Map[String, Seq[EmittedParts]] = {
    val sortedNames: Seq[Seq[String]] = Utils.topologicalSortWithGrouping(data.dependencies)
    val nameToEmittedClass = data.parts.map(p => p.name -> p).toMap
    val sorted: Seq[Seq[EmittedParts]] = sortedNames.map {_.map {nameToEmittedClass(_)}}
    sorted.map { case sortedPartsSeq =>
      val masterPartFileName = sortedPartsSeq.head.inFile
      (masterPartFileName, sortedPartsSeq.map {_.copy(inFile = masterPartFileName)})
    }.toMap
  }
}

case object CodeWriterSuccess
