package tk.monnef.dia2scala

import java.util.zip.GZIPInputStream
import scala.util.Try
import java.io.{InputStreamReader, FileInputStream, InputStream, File}
import scala.util.control.NonFatal
import Utils._

object XmlParser {

  import XmlParserHelper._
  import scalaz._
  import Scalaz._

  def parseFile(file: File, isPacked: Boolean): \/[String, DiaFile] = {
    getXmlStream(file, isPacked) match {
      case scala.util.Failure(t) => s"Unable to create stream reader: ${t.getMessage}".left
      case scala.util.Success(xmlStream) =>
        val xml = scala.xml.XML.load(xmlStream)
        /*
        * Process list:
        * - xml
        * - parse and process packages
        * - parse and semi-process classes: name, compute package, attributes (not yet from association connections) and operators
        * - parse and process generalization
        * - parse and process <<implements>>
        * - parse and process <<mixin>>
        * - parse and process associations
        */
        for {
          pcks <- processPackages(xml)
        } yield DiaFile(pcks, Seq())
    }
  }
}

object XmlParserHelper {

  import scala.xml.{Node, NodeSeq, Elem}
  import scalaz._
  import scalaz.syntax.std.tuple._
  import scalaz.syntax.either._
  import Scalaz.ToIdOps

  final val DiaNodeTypeObject = "object"
  final val DiaNodeTypeAttribute = "attribute"
  final val DiaNodeTypePoint = "point"
  final val DiaNodeTypeReal = "real"
  final val DiaNodeTypeString = "string"

  final val DiaObjectTypePackage = "UML - LargePackage"

  final val DiaAttributeName = "name"
  final val DiaAttributeType = "type"
  final val DiaAttributeVal = "val"

  final val StringBarrier = '#'


  def getXmlStream(file: File, isPacked: Boolean): Try[InputStreamReader] =
    Try {
      new FileInputStream(file).
        |> { a => if (isPacked) new GZIPInputStream(a) else a: InputStream}.
        |> {new InputStreamReader(_)}
    }

  def extractObjectsByType(e: Elem, objType: String): NodeSeq = e \\ DiaNodeTypeObject filter {_ \@ DiaAttributeType == objType}

  def extractDiaAttributeByName(n: Node, name: String): Node = (n \\ DiaNodeTypeAttribute filter {_ \@ DiaAttributeName == name}).head

  def extractDiaAttributePoint(n: Node, name: String): (Double, Double) =
    (extractDiaAttributeByName(n, name) \ DiaNodeTypePoint \@ DiaAttributeVal).
      split(',').toSeq.take(2).map(_.toDouble) |> seqOfTwoToTuple

  def extractDiaAttributeReal(n: Node, name: String): Double =
    (extractDiaAttributeByName(n, name) \ DiaNodeTypeReal \@ DiaAttributeVal).toDouble

  def extractDiaAttributeString(n: Node, name: String): String =
    (extractDiaAttributeByName(n, name) \ DiaNodeTypeString).text


  def extractDiaAttributeStringAndStrip(n: Node, name: String): String = {
    val str = extractDiaAttributeString(n, name)
    if (str.length < 2) throw new RuntimeException(s"String is too short to contain even barriers '$str'.\n$n")
    if (str.head != StringBarrier || str.last != StringBarrier) throw new RuntimeException(s"String is missing some/both barriers '$str'.\n$n")
    str.drop(1).dropRight(1)
  }


  def extractDiaAttributeBoolean(n: Node, name: String): Boolean =
    (extractDiaAttributeByName(n, name) \ "dia:boolean" \@ "val").toLowerCase match {
      case "true" => true
      case "false" => false
      case s => throw new RuntimeException(s"Cannot parse boolean string '$s'.")
    }


  def processPackage(n: Node): \/[String, DiaPackage] =
    wrapErrorToJunction {
      val typeAttr = n \@ DiaAttributeType
      val nodeLabel = n.label
      if (nodeLabel != DiaNodeTypeObject || typeAttr != DiaObjectTypePackage)
        throw new RuntimeException(s"Node is not a package ($nodeLabel, $typeAttr)!\n$n")
      val (x, y) = extractDiaAttributePoint(n, "elem_corner")
      val w = extractDiaAttributeReal(n, "elem_width")
      val h = extractDiaAttributeReal(n, "elem_height")
      val name = extractDiaAttributeStringAndStrip(n, "name")

      val p = DiaPackage(name, DiaGeometry(x, y, w, h))
      println(s"Returning package: $p")
      p
    }

  def processPackages(e: Elem): \/[String, Seq[DiaPackage]] = {
    val packages: Seq[\/[String, DiaPackage]] = extractObjectsByType(e, DiaObjectTypePackage).map(processPackage)
    packages |> liftFirstError
  }
}
