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

  it should "read properly simple package info from packed file" in {
    rightOrFailIn(parsePacked("simple01")) { res =>
      val p = res.packages
      assert(p.size == 1)
      val a = p.seq.head
      assert(a == DiaPackage("packageB", DiaGeometry(11, 1, 17, 7)))
    }
  }

  it should "read classes and its packages" in {
    rightOrFailIn(parsePacked("simple01")) { res =>
      assert(res.packages.size == 1)
      assert(res.classes.size == 2, res.classes)
      assert(res.classes.find(_.name == "ClassA").get.inPackage == "")
      assert(res.classes.find(_.name == "ClassB").get.inPackage == "packageB")
    }
  }

  final val xmlPackage = "    <dia:object type=\"UML - LargePackage\" version=\"0\" id=\"O0\">\n      <dia:attribute name=\"obj_pos\">\n        <dia:point val=\"11,1\"/>\n      </dia:attribute>\n      <dia:attribute name=\"obj_bb\">\n        <dia:rectangle val=\"10.95,-0.05;28.05,8.05\"/>\n      </dia:attribute>\n      <dia:attribute name=\"meta\">\n        <dia:composite type=\"dict\"/>\n      </dia:attribute>\n      <dia:attribute name=\"elem_corner\">\n        <dia:point val=\"11,1\"/>\n      </dia:attribute>\n      <dia:attribute name=\"elem_width\">\n        <dia:real val=\"17\"/>\n      </dia:attribute>\n      <dia:attribute name=\"elem_height\">\n        <dia:real val=\"7\"/>\n      </dia:attribute>\n      <dia:attribute name=\"line_width\">\n        <dia:real val=\"0.10000000149011612\"/>\n      </dia:attribute>\n      <dia:attribute name=\"line_colour\">\n        <dia:color val=\"#000000\"/>\n      </dia:attribute>\n      <dia:attribute name=\"fill_colour\">\n        <dia:color val=\"#ffffff\"/>\n      </dia:attribute>\n      <dia:attribute name=\"text_colour\">\n        <dia:color val=\"#000000\"/>\n      </dia:attribute>\n      <dia:attribute name=\"stereotype\">\n        <dia:string>##</dia:string>\n      </dia:attribute>\n      <dia:attribute name=\"name\">\n        <dia:string>#packageB#</dia:string>\n      </dia:attribute>\n    </dia:object> "
  "processPackage" should "process a package" in {
    val elem = xml.XML.loadString(xmlPackage)
    rightOrFailIn(XmlParserHelper.processPackage(elem)) { res =>
      assert(res == DiaPackage("packageB", DiaGeometry(11, 1, 17, 7)))
    }
  }

  final val xmlAttribute = "       <dia:composite type=\"umlattribute\">\n          <dia:attribute name=\"name\">\n            <dia:string>#aReference#</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"type\">\n            <dia:string>#ClassA#</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"value\">\n            <dia:string>##</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"comment\">\n            <dia:string>##</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"visibility\">\n            <dia:enum val=\"2\"/>\n          </dia:attribute>\n          <dia:attribute name=\"abstract\">\n            <dia:boolean val=\"false\"/>\n          </dia:attribute>\n          <dia:attribute name=\"class_scope\">\n            <dia:boolean val=\"false\"/>\n          </dia:attribute> </dia:composite>"
  final val expectedAttributeA = DiaAttribute("aReference", Some("ClassA"), DiaVisibility.Protected)
  "processAttribute" should "process an attribute" in {
    val elem = xml.XML.loadString(xmlAttribute)
    val res = XmlParserHelper.processAttribute(elem)
    assert(res == expectedAttributeA)
  }

  final val xmlOperation = "        <dia:composite type=\"umloperation\">\n          <dia:attribute name=\"name\">\n            <dia:string>#stringToInt#</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"stereotype\">\n            <dia:string>##</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"type\">\n            <dia:string>#Int#</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"visibility\">\n            <dia:enum val=\"0\"/>\n          </dia:attribute>\n          <dia:attribute name=\"comment\">\n            <dia:string>##</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"abstract\">\n            <dia:boolean val=\"false\"/>\n          </dia:attribute>\n          <dia:attribute name=\"inheritance_type\">\n            <dia:enum val=\"2\"/>\n          </dia:attribute>\n          <dia:attribute name=\"query\">\n            <dia:boolean val=\"false\"/>\n          </dia:attribute>\n          <dia:attribute name=\"class_scope\">\n            <dia:boolean val=\"false\"/>\n          </dia:attribute>\n          <dia:attribute name=\"parameters\">\n            <dia:composite type=\"umlparameter\">\n              <dia:attribute name=\"name\">\n                <dia:string>#str#</dia:string>\n              </dia:attribute>\n              <dia:attribute name=\"type\">\n                <dia:string>#String#</dia:string>\n              </dia:attribute>\n              <dia:attribute name=\"value\">\n                <dia:string>##</dia:string>\n              </dia:attribute>\n              <dia:attribute name=\"comment\">\n                <dia:string>##</dia:string>\n              </dia:attribute>\n              <dia:attribute name=\"kind\">\n                <dia:enum val=\"0\"/>\n              </dia:attribute>\n            </dia:composite>\n          </dia:attribute>\n        </dia:composite>"
  final val expectedOperation = DiaOperationDescriptor("stringToInt", DiaVisibility.Public, Seq(DiaOperationParameter("str", "String".some)), "Int".some)
  "processOperation" should "process an operation" in {
    val elem = xml.XML.loadString(xmlOperation)
    val res = XmlParserHelper.processOperation(elem)
    assert(res == expectedOperation)
  }

  final val xmlClass = "  <dia:object type=\"UML - Class\" version=\"0\" id=\"O2\">\n      <dia:attribute name=\"obj_pos\">\n        <dia:point val=\"13,3\"/>\n      </dia:attribute>\n      <dia:attribute name=\"obj_bb\">\n        <dia:rectangle val=\"12.95,2.95;24.715,6.45\"/>\n      </dia:attribute>\n      <dia:attribute name=\"elem_corner\">\n        <dia:point val=\"13,3\"/>\n      </dia:attribute>\n      <dia:attribute name=\"elem_width\">\n        <dia:real val=\"11.665000000000001\"/>\n      </dia:attribute>\n      <dia:attribute name=\"elem_height\">\n        <dia:real val=\"3.3999999999999999\"/>\n      </dia:attribute>\n      <dia:attribute name=\"name\">\n        <dia:string>#ClassB#</dia:string>\n      </dia:attribute>\n      <dia:attribute name=\"stereotype\">\n        <dia:string>##</dia:string>\n      </dia:attribute>\n      <dia:attribute name=\"comment\">\n        <dia:string>##</dia:string>\n      </dia:attribute>\n      <dia:attribute name=\"abstract\">\n        <dia:boolean val=\"false\"/>\n      </dia:attribute>\n      <dia:attribute name=\"suppress_attributes\">\n        <dia:boolean val=\"false\"/>\n      </dia:attribute>\n      <dia:attribute name=\"suppress_operations\">\n        <dia:boolean val=\"false\"/>\n      </dia:attribute>\n      <dia:attribute name=\"visible_attributes\">\n        <dia:boolean val=\"true\"/>\n      </dia:attribute>\n      <dia:attribute name=\"visible_operations\">\n        <dia:boolean val=\"true\"/>\n      </dia:attribute>\n      <dia:attribute name=\"visible_comments\">\n        <dia:boolean val=\"false\"/>\n      </dia:attribute>\n      <dia:attribute name=\"wrap_operations\">\n        <dia:boolean val=\"true\"/>\n      </dia:attribute>\n      <dia:attribute name=\"wrap_after_char\">\n        <dia:int val=\"40\"/>\n      </dia:attribute>\n      <dia:attribute name=\"comment_line_length\">\n        <dia:int val=\"17\"/>\n      </dia:attribute>\n      <dia:attribute name=\"comment_tagging\">\n        <dia:boolean val=\"false\"/>\n      </dia:attribute>\n      <dia:attribute name=\"line_width\">\n        <dia:real val=\"0.10000000000000001\"/>\n      </dia:attribute>\n      <dia:attribute name=\"line_color\">\n        <dia:color val=\"#000000\"/>\n      </dia:attribute>\n      <dia:attribute name=\"fill_color\">\n        <dia:color val=\"#ffffff\"/>\n      </dia:attribute>\n      <dia:attribute name=\"text_color\">\n        <dia:color val=\"#000000\"/>\n      </dia:attribute>\n      <dia:attribute name=\"normal_font\">\n        <dia:font family=\"monospace\" style=\"0\" name=\"Courier\"/>\n      </dia:attribute>\n      <dia:attribute name=\"abstract_font\">\n        <dia:font family=\"monospace\" style=\"0\" name=\"Courier\"/>\n      </dia:attribute>\n      <dia:attribute name=\"polymorphic_font\">\n        <dia:font family=\"monospace\" style=\"0\" name=\"Courier\"/>\n      </dia:attribute>\n      <dia:attribute name=\"classname_font\">\n        <dia:font family=\"sans\" style=\"80\" name=\"Helvetica-Bold\"/>\n      </dia:attribute>\n      <dia:attribute name=\"abstract_classname_font\">\n        <dia:font family=\"sans\" style=\"0\" name=\"Helvetica\"/>\n      </dia:attribute>\n      <dia:attribute name=\"comment_font\">\n        <dia:font family=\"sans\" style=\"0\" name=\"Helvetica\"/>\n      </dia:attribute>\n      <dia:attribute name=\"normal_font_height\">\n        <dia:real val=\"0.80000000000000004\"/>\n      </dia:attribute>\n      <dia:attribute name=\"polymorphic_font_height\">\n        <dia:real val=\"0.80000000000000004\"/>\n      </dia:attribute>\n      <dia:attribute name=\"abstract_font_height\">\n        <dia:real val=\"0.80000000000000004\"/>\n      </dia:attribute>\n      <dia:attribute name=\"classname_font_height\">\n        <dia:real val=\"1\"/>\n      </dia:attribute>\n      <dia:attribute name=\"abstract_classname_font_height\">\n        <dia:real val=\"1\"/>\n      </dia:attribute>\n      <dia:attribute name=\"comment_font_height\">\n        <dia:real val=\"0.69999999999999996\"/>\n      </dia:attribute>\n      <dia:attribute name=\"attributes\">\n        <dia:composite type=\"umlattribute\">\n          <dia:attribute name=\"name\">\n            <dia:string>#aReference#</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"type\">\n            <dia:string>#ClassA#</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"value\">\n            <dia:string>##</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"comment\">\n            <dia:string>##</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"visibility\">\n            <dia:enum val=\"2\"/>\n          </dia:attribute>\n          <dia:attribute name=\"abstract\">\n            <dia:boolean val=\"false\"/>\n          </dia:attribute>\n          <dia:attribute name=\"class_scope\">\n            <dia:boolean val=\"false\"/>\n          </dia:attribute>\n        </dia:composite>\n      </dia:attribute>\n      <dia:attribute name=\"operations\">\n        <dia:composite type=\"umloperation\">\n          <dia:attribute name=\"name\">\n            <dia:string>#stringToInt#</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"stereotype\">\n            <dia:string>##</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"type\">\n            <dia:string>#Int#</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"visibility\">\n            <dia:enum val=\"0\"/>\n          </dia:attribute>\n          <dia:attribute name=\"comment\">\n            <dia:string>##</dia:string>\n          </dia:attribute>\n          <dia:attribute name=\"abstract\">\n            <dia:boolean val=\"false\"/>\n          </dia:attribute>\n          <dia:attribute name=\"inheritance_type\">\n            <dia:enum val=\"2\"/>\n          </dia:attribute>\n          <dia:attribute name=\"query\">\n            <dia:boolean val=\"false\"/>\n          </dia:attribute>\n          <dia:attribute name=\"class_scope\">\n            <dia:boolean val=\"false\"/>\n          </dia:attribute>\n          <dia:attribute name=\"parameters\">\n            <dia:composite type=\"umlparameter\">\n              <dia:attribute name=\"name\">\n                <dia:string>#str#</dia:string>\n              </dia:attribute>\n              <dia:attribute name=\"type\">\n                <dia:string>#String#</dia:string>\n              </dia:attribute>\n              <dia:attribute name=\"value\">\n                <dia:string>##</dia:string>\n              </dia:attribute>\n              <dia:attribute name=\"comment\">\n                <dia:string>##</dia:string>\n              </dia:attribute>\n              <dia:attribute name=\"kind\">\n                <dia:enum val=\"0\"/>\n              </dia:attribute>\n            </dia:composite>\n          </dia:attribute>\n        </dia:composite>\n      </dia:attribute>\n      <dia:attribute name=\"template\">\n        <dia:boolean val=\"false\"/>\n      </dia:attribute>\n      <dia:attribute name=\"templates\"/>\n    </dia:object>"
  "processClass" should "process a class" in {
    val elem = xml.XML.loadString(xmlClass)
    rightOrFailIn(XmlParserHelper.processClass(elem)) { res =>
      assert(res == DiaClass("ClassB", DiaGeometry(13, 3, 11.665000000000001, 3.3999999999999999), "", "", Seq(), "O2", Seq(expectedAttributeA), Seq(expectedOperation), DiaClassType.Class))
    }
  }

  final val xmlConnections = "      <dia:connections>\n        <dia:connection handle=\"0\" to=\"O2\" connection=\"0\"/>\n        <dia:connection handle=\"1\" to=\"O1\" connection=\"8\"/>\n      </dia:connections>"
  "parseConnections" should "parse a connection node" in {
    val elem = xml.XML.loadString(xmlConnections)
    val (fromId, toId) = XmlParserHelper.parseConnections(elem)
    assert(fromId == "O1")
    assert(toId == "O2")
  }

  final val xmlGeneralization = "    <dia:object type=\"UML - Generalization\" version=\"1\" id=\"O3\">\n      <dia:attribute name=\"obj_pos\">\n        <dia:point val=\"13,3\"/>\n      </dia:attribute>\n      <dia:attribute name=\"obj_bb\">\n        <dia:rectangle val=\"6.47294,2.15;13.05,7.15\"/>\n      </dia:attribute>\n      <dia:attribute name=\"meta\">\n        <dia:composite type=\"dict\"/>\n      </dia:attribute>\n      <dia:attribute name=\"orth_points\">\n        <dia:point val=\"13,3\"/>\n        <dia:point val=\"9.36147,3\"/>\n        <dia:point val=\"9.36147,7.1\"/>\n        <dia:point val=\"6.52294,7.1\"/>\n      </dia:attribute>\n      <dia:attribute name=\"orth_orient\">\n        <dia:enum val=\"0\"/>\n        <dia:enum val=\"1\"/>\n        <dia:enum val=\"0\"/>\n      </dia:attribute>\n      <dia:attribute name=\"orth_autoroute\">\n        <dia:boolean val=\"true\"/>\n      </dia:attribute>\n      <dia:attribute name=\"text_colour\">\n        <dia:color val=\"#000000\"/>\n      </dia:attribute>\n      <dia:attribute name=\"line_colour\">\n        <dia:color val=\"#000000\"/>\n      </dia:attribute>\n      <dia:attribute name=\"name\">\n        <dia:string>##</dia:string>\n      </dia:attribute>\n      <dia:attribute name=\"stereotype\">\n        <dia:string>##</dia:string>\n      </dia:attribute>\n      <dia:connections>\n        <dia:connection handle=\"0\" to=\"O2\" connection=\"0\"/>\n        <dia:connection handle=\"1\" to=\"O1\" connection=\"8\"/>\n      </dia:connections>\n    </dia:object>"
  "parseGeneralization" should "parse a valid generalization" in {
    val elem = xml.XML.loadString(xmlGeneralization)
    val res = XmlParserHelper.parseGeneralization(elem)
    assert(res == DiaOneWayConnection("O1", "O2", DiaGeneralizationType).some)
  }

  final val xmlRealises = "    <dia:object type=\"UML - Realizes\" version=\"1\" id=\"O17\">\n      <dia:attribute name=\"obj_pos\">\n        <dia:point val=\"14.9825,32.0504\"/>\n      </dia:attribute>\n      <dia:attribute name=\"obj_bb\">\n        <dia:rectangle val=\"14.1325,32.0004;17.7263,36\"/>\n      </dia:attribute>\n      <dia:attribute name=\"meta\">\n        <dia:composite type=\"dict\"/>\n      </dia:attribute>\n      <dia:attribute name=\"orth_points\">\n        <dia:point val=\"14.9825,32.0504\"/>\n        <dia:point val=\"14.9825,34.4\"/>\n        <dia:point val=\"17.6763,34.4\"/>\n        <dia:point val=\"17.6763,35.9495\"/>\n      </dia:attribute>\n      <dia:attribute name=\"orth_orient\">\n        <dia:enum val=\"1\"/>\n        <dia:enum val=\"0\"/>\n        <dia:enum val=\"1\"/>\n      </dia:attribute>\n      <dia:attribute name=\"orth_autoroute\">\n        <dia:boolean val=\"true\"/>\n      </dia:attribute>\n      <dia:attribute name=\"line_colour\">\n        <dia:color val=\"#000000\"/>\n      </dia:attribute>\n      <dia:attribute name=\"text_colour\">\n        <dia:color val=\"#000000\"/>\n      </dia:attribute>\n      <dia:attribute name=\"name\">\n        <dia:string>##</dia:string>\n      </dia:attribute>\n      <dia:attribute name=\"stereotype\">\n        <dia:string>##</dia:string>\n      </dia:attribute>\n      <dia:connections>\n        <dia:connection handle=\"0\" to=\"O13\" connection=\"8\"/>\n        <dia:connection handle=\"1\" to=\"O16\" connection=\"10\"/>\n      </dia:connections>\n      <dia:childnode parent=\"O5\"/>\n    </dia:object>"
  "parseRealizes" should "parse a valid generalization" in {
    val elem = xml.XML.loadString(xmlRealises)
    val res = XmlParserHelper.parseRealizes(elem)
    assert(res == DiaOneWayConnection("O16", "O13", DiaImplementsType).some)
  }
}
