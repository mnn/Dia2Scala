package tk.monnef.dia2scala

import scalaz._
import Scalaz._
import java.io.File

object XmlParser {
  def parseFile(file: File, isPacked: Boolean): \/[String, DiaFile] = {
    // TODO
    DiaFile(Seq(), Seq()).right
  }
}
