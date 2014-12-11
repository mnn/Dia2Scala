package tk.monnef.dia2scala

import java.io.File

import scalaz._
import Scalaz._
import org.scalatest.FlatSpec

class TestXmlParser extends FlatSpec {
  "parseFile" should "read empty diagram" in {
    XmlParser.parseFile(new File(TestHelper.genDiagramPath("empty")), isPacked = true) match {
      case \/-(res) =>
        res.classes.isEmpty
        res.packages.isEmpty
      case -\/(err) => fail(s"Error occurred (current path: ${TestHelper.currentPath()}): $err")
    }
  }
}
