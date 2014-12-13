package tk.monnef.dia2scala

import java.util.zip.GZIPInputStream
import tk.monnef.dia2scala.DiaVisibility.DiaVisibility

import scala.util.Try
import java.io.{InputStreamReader, FileInputStream, InputStream, File}
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
        * - parse and semi-process classes:
        *     - name, attributes (not yet from association connections) and operators
         *    - handle <<enumeration>>
        *     - compute package
        * - parse and process generalization
        * - parse and process <<implements>>
        * - parse and process <<mixin>>
        * - parse and process associations
        */
        for {
          a <- processPackages(xml, DiaFile())
          b <- semiProcessClasses(xml, a)
        } yield a
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
  final val DiaNodeTypeBoolean = "boolean"
  final val DiaNodeTypeComposite = "composite"
  final val DiaNodeTypeEnum = "enum"

  final val DiaObjectTypePackage = "UML - LargePackage"

  final val DiaCompositeTypeUMLAttribute = "umlattribute"
  final val DiaCompositeTypeUMLOperation = "umloperation"

  final val DiaAttributeName = "name"
  final val DiaAttributeType = "type"
  final val DiaAttributeVal = "val"
  final val DiaAttributeStereotype = "stereotype"
  final val DiaAttributeAttributes = "attributes"
  final val DiaAttributeOperations = "operations"
  final val DiaAttributeTemplate = "template"
  final val DiaAttributeTemplates = "templates"
  final val DiaAttributeValue = "value"
  final val DiaAttributeVisibility = "visibility"
  final val DiaAttributeId = "id"

  final val StringBarrier = '#'

  def getXmlStream(file: File, isPacked: Boolean): Try[InputStreamReader] =
    Try {
      new FileInputStream(file).
        |> { a => if (isPacked) new GZIPInputStream(a) else a: InputStream}.
        |> {new InputStreamReader(_)}
    }

  def extractObjectsByType(e: Elem, objType: String): NodeSeq = e \\ DiaNodeTypeObject filter {_ \@ DiaAttributeType == objType}

  def extractDiaAttributesMatchingName(n: Node, name: String): Seq[Node] = (n \\ DiaNodeTypeAttribute filter {_ \@ DiaAttributeName == name})

  def extractDiaAttributeByName(n: Node, name: String): Node = extractDiaAttributesMatchingName(n, name).head

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
    (extractDiaAttributeByName(n, name) \ DiaNodeTypeBoolean \@ DiaAttributeVal).toLowerCase match {
      case "true" => true
      case "false" => false
      case s => throw new RuntimeException(s"Cannot parse boolean string '$s'.")
    }

  def extractDiaAttributeEnum(n: Node, name: String): Int =
    (extractDiaAttributeByName(n, name) \ DiaNodeTypeEnum \@ DiaAttributeVal).toInt

  def extractGeometry(n: Node): DiaGeometry = {
    val (x, y) = extractDiaAttributePoint(n, "elem_corner")
    val w = extractDiaAttributeReal(n, "elem_width")
    val h = extractDiaAttributeReal(n, "elem_height")
    DiaGeometry(x, y, w, h)
  }

  def extractVisibility(n: Node, name: String = DiaAttributeVisibility): DiaVisibility =
    extractDiaAttributeEnum(n, name) |> {DiaVisibility.apply}

  def processPackage(n: Node): \/[String, DiaPackage] =
    wrapErrorToJunction {
      val typeAttr = n \@ DiaAttributeType
      val nodeLabel = n.label
      if (nodeLabel != DiaNodeTypeObject || typeAttr != DiaObjectTypePackage)
        throw new RuntimeException(s"Node is not a package ($nodeLabel, $typeAttr)!\n$n")
      val name = extractDiaAttributeStringAndStrip(n, DiaAttributeName)
      DiaPackage(name, extractGeometry(n))
    }

  def processPackages(e: Elem, f: DiaFile): \/[String, DiaFile] = {
    val packages: \/[String, Seq[DiaPackage]] = extractObjectsByType(e, DiaObjectTypePackage).map(processPackage) |> liftFirstError
    packages.map { p => f.copy(packages = p)}
  }

  def processAttribute(n: Node): DiaAttribute = {
    val attributeName = n \@ DiaAttributeType
    if (attributeName != DiaCompositeTypeUMLAttribute) throw new RuntimeException(s"Attribute name is not an $DiaCompositeTypeUMLAttribute, but $attributeName.\n$n")
    DiaAttribute(
      extractDiaAttributeStringAndStrip(n, DiaAttributeName),
      extractDiaAttributeStringAndStrip(n, DiaAttributeType) |> wrapNonEmptyStringToSome,
      extractVisibility(n)
    )
  }

  def processClass(n: Node): \/[String, DiaClass] =
    wrapErrorToJunction {
      val stereotype = extractDiaAttributeStringAndStrip(n, DiaAttributeStereotype)

      val attributes = extractDiaAttributesMatchingName(n, DiaAttributeAttributes).map(processAttribute)
      val operations = Seq()

      DiaClass(
        extractDiaAttributeStringAndStrip(n, DiaAttributeName),
        extractGeometry(n),
        "",
        "",
        Seq(),
        n \@ DiaAttributeId,
        attributes,
        operations
      )
    }

  def semiProcessClasses(e: Elem, f: DiaFile): \/[String, DiaFile] = {
    val classes = extractObjectsByType(e, DiaObjectTypePackage).map(processClass) |> liftFirstError
    classes.map { c => f.copy(classes = c)}
  }
}
