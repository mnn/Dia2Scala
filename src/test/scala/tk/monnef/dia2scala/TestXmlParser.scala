package tk.monnef.dia2scala

import java.io.File

import scalaz._
import Scalaz._
import org.scalatest.FlatSpec

class TestXmlParser extends FlatSpec {
  def rightOrFailIn[A](in: \/[String, A])(c: A => Unit) {
    in.fold(e => fail(s"Error occurred (current path: ${TestHelper.currentPath()}): $e"), c)
  }

  def parse(name: String, packed: Boolean): \/[String, DiaFile] =
    XmlParser.parseFile(new File(TestHelper.genDiagramPath(name)), packed)

  def parsePacked(name: String): \/[String, DiaFile] = parse(name, packed = true)

  def parseUnpacked(name: String): \/[String, DiaFile] = parse(name, packed = false)

  "parseFile" should "read empty diagram" in {
    rightOrFailIn(parsePacked("empty")) { res =>
      assert(res.classes.isEmpty)
      assert(res.packages.isEmpty)
    }
  }

  it should "read properly simple package info from unpacked file" in {
    rightOrFailIn(parseUnpacked("simple01u")) { res =>
      val p = res.packages
      assert(p.size == 1)
      val a = p.seq.head
      assert(a == DiaPackage("packageB", DiaGeometry(11, 1, 17, 7)))
    }
  }

  it should "read properly simple package info  from packed file" in {
    rightOrFailIn(parsePacked("simple01")) { res =>
      val p = res.packages
      assert(p.size == 1)
      val a = p.seq.head
      assert(a == DiaPackage("packageB", DiaGeometry(11, 1, 17, 7)))
    }
  }

  final val xmlPackage = "    <dia:object type=\"UML - LargePackage\" version=\"0\" id=\"O0\">\n      <dia:attribute name=\"obj_pos\">\n        <dia:point val=\"11,1\"/>\n      </dia:attribute>\n      <dia:attribute name=\"obj_bb\">\n        <dia:rectangle val=\"10.95,-0.05;28.05,8.05\"/>\n      </dia:attribute>\n      <dia:attribute name=\"meta\">\n        <dia:composite type=\"dict\"/>\n      </dia:attribute>\n      <dia:attribute name=\"elem_corner\">\n        <dia:point val=\"11,1\"/>\n      </dia:attribute>\n      <dia:attribute name=\"elem_width\">\n        <dia:real val=\"17\"/>\n      </dia:attribute>\n      <dia:attribute name=\"elem_height\">\n        <dia:real val=\"7\"/>\n      </dia:attribute>\n      <dia:attribute name=\"line_width\">\n        <dia:real val=\"0.10000000149011612\"/>\n      </dia:attribute>\n      <dia:attribute name=\"line_colour\">\n        <dia:color val=\"#000000\"/>\n      </dia:attribute>\n      <dia:attribute name=\"fill_colour\">\n        <dia:color val=\"#ffffff\"/>\n      </dia:attribute>\n      <dia:attribute name=\"text_colour\">\n        <dia:color val=\"#000000\"/>\n      </dia:attribute>\n      <dia:attribute name=\"stereotype\">\n        <dia:string>##</dia:string>\n      </dia:attribute>\n      <dia:attribute name=\"name\">\n        <dia:string>#packageB#</dia:string>\n      </dia:attribute>\n    </dia:object> "

  "processPackage" should "process a package" in {
    val elem = xml.XML.loadString(xmlPackage)
    rightOrFailIn(XmlParserHelper.processPackage(elem)) { res =>
      assert(res == DiaPackage("packageB", DiaGeometry(11, 1, 17, 7)))
    }
  }
}
