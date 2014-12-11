package tk.monnef.dia2scala

import scalaz._
import Scalaz._

object CodeWriter {
  def writeTextFile(data: String): \/[String, CodeWriterSuccess.type] = {
    // TODO
    CodeWriterSuccess.right
  }
}

case object CodeWriterSuccess
