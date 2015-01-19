package tk.monnef.dia2scala

import java.io.File

import scala.util.control.NonFatal
import scalaz._
import Scalaz._
import Utils._

// TODO: [low priority][improvement] fix objects not being grouped with its classes/traits

object CodeWriter {
  def writeTextFile(data: EmittedCode, path: String, groupByDependency: Boolean): \/[String, CodeWriterSuccess.type] = {
    Log.printDebug(s"CodeWriter.writeTextFile:")
    //try {
    new File(path).mkdirs()

    val inFileToClasses =
      if (!groupByDependency) data.parts.groupBy(_.inFile)
      else groupAndSortByInheritance(data)

    Log.printTrace(s"inFileToClasses, required classes view:\n" + inFileToClasses.map { case (scFile, classes) => scFile + ":\n" + classes.map { case c => c.fullName + " -> " + c.requiredClasses}.mkString("\n") + "\n"}.mkString("\n") + "\n")

    val inFileToClassesWithImports = inFileToClasses.map { case (scFile, classes) =>
      (scFile, classes,
        classes.flatMap(_.requiredClasses).distinct.filter(i => !classes.exists(_.fullName == i)).sorted
        )
    }

    Log.printTrace(s"inFileToClassesWithImports = \n${inFileToClassesWithImports.toString()}")

    for {(scFile, classes, imports) <- inFileToClassesWithImports} {
      val c = classes.head
      val pckgPath = path + "/" + c.inPackage.split("\\.").mkString("/")
      new File(pckgPath).mkdirs()
      val outFile = new File(pckgPath + "/" + scFile)
      printToFile(outFile) { p =>
        if (c.inPackage.nonEmpty) p.println(s"package ${c.inPackage}\n")

        if (imports.nonEmpty) {
          imports.foreach(i => p.println("import " + i))
          p.println()
        }

        classes.map(_.code).mkString("\n\n") |> p.print
      }
    }
    CodeWriterSuccess.right
    /*} catch {
      case NonFatal(e) => e.getMessage.left
    } */
  }

  def groupAndSortByInheritance(data: EmittedCode): Map[String, Seq[EmittedParts]] = {
    val sortedNames: Seq[Seq[String]] = Utils.topologicalSortWithGrouping(data.dependencies)
    val nameToEmittedClass = data.parts.map(p => p.fullName -> p).toMap
    val sorted: Seq[Seq[EmittedParts]] = sortedNames.map {_.map {nameToEmittedClass(_)}}
    val sortedAndGrouped: Seq[Seq[EmittedParts]] = breakGroupsContainingDifferentPackages(sorted)

    val notDependent: Map[String, Seq[EmittedParts]] = data.parts.filter { part =>
      val pName = part.fullName
      !data.dependencies.exists { case (a, b) => a == pName || b == pName}
    }.map { part => (part.inFile, Seq(part))}.toMap

    sortedAndGrouped.map { case sortedPartsSeq =>
      val masterPartFileName = sortedPartsSeq.head.inFile
      (masterPartFileName, sortedPartsSeq.map {_.copy(inFile = masterPartFileName)})
    }.toMap ++ notDependent
  }

  def breakGroupsContainingDifferentPackages(d: Seq[Seq[EmittedParts]]): Seq[Seq[EmittedParts]] =
    if (d.isEmpty) Seq()
    else d.foldLeft(Seq(Seq[EmittedParts]())) { case (acc: Seq[Seq[EmittedParts]], group: Seq[EmittedParts]) =>
      group.foldLeft(acc) { case (innerAcc: Seq[Seq[EmittedParts]], item: EmittedParts) =>
        val lastGroup: Seq[EmittedParts] = innerAcc.last
        if (lastGroup.isEmpty || lastGroup.head.inPackage == item.inPackage) innerAcc.init :+ (innerAcc.last :+ item)
        else innerAcc :+ Seq(item)
      }
    }
}

case object CodeWriterSuccess
