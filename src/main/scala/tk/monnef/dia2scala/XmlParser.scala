package tk.monnef.dia2scala

import java.util.zip.GZIPInputStream
import tk.monnef.dia2scala.DiaFile.convertTypeWithoutClassExistenceChecks
import tk.monnef.dia2scala.DiaVisibility.DiaVisibility

import scala.util.Try
import java.io.{InputStreamReader, FileInputStream, InputStream, File}
import Utils._
import DiaClassType._

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
         *    - handle <<enumeration>> TODO
        *     - compute package
        * - parse and process generalization
        * - parse and process <<implements>>, <<mixin>>, <<hasA>> ~ <<companionOf>>  TODO
        * - parse and process associations TODO
        */
        for {
          a <- processPackages(xml, DiaFile())
          b <- semiProcessClasses(xml, a)
          c <- processOneWayConnections(xml, b)
        } yield c
    }
  }
}

object XmlParserHelper {

  import scala.xml.{Node, NodeSeq, Elem}
  import scalaz._
  import scalaz.syntax.std.tuple._
  import scalaz.syntax.either._
  import scalaz.syntax.std.option._
  import Scalaz.ToIdOps

  final val DiaNodeTypeObject = "object"
  final val DiaNodeTypeAttribute = "attribute"
  final val DiaNodeTypePoint = "point"
  final val DiaNodeTypeReal = "real"
  final val DiaNodeTypeString = "string"
  final val DiaNodeTypeBoolean = "boolean"
  final val DiaNodeTypeComposite = "composite"
  final val DiaNodeTypeEnum = "enum"
  final val DiaNodeTypeConnections = "connections"
  final val DiaNodeTypeConnection = "connection"

  final val DiaObjectTypePackage = "UML - LargePackage"
  final val DiaObjectTypeClass = "UML - Class"
  final val DiaObjectTypeGeneralization = "UML - Generalization"
  final val DiaObjectTypeRealizes = "UML - Realizes"

  final val DiaCompositeTypeUMLAttribute = "umlattribute"
  final val DiaCompositeTypeUMLOperation = "umloperation"
  final val DiaCompositeTypeUMLParameter = "umlparameter"

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
  final val DiaAttributeParameters = "parameters"

  final val StringBarrier = '#'

  def getXmlStream(file: File, isPacked: Boolean): Try[InputStreamReader] =
    Try {
      new FileInputStream(file).
        |> { a => if (isPacked) new GZIPInputStream(a) else a: InputStream}.
        |> {new InputStreamReader(_)}
    }

  def extractObjectsByType(e: Node, objType: String): NodeSeq = e \\ DiaNodeTypeObject filter {_ \@ DiaAttributeType == objType}

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
      assertNodeName(n, DiaNodeTypeObject)
      assertAttributeType(n, DiaObjectTypePackage)
      DiaPackage(extractAttributeName(n), extractGeometry(n))
    }

  def processPackages(e: Elem, f: DiaFile): \/[String, DiaFile] = {
    val packages: \/[String, Seq[DiaPackage]] = extractObjectsByType(e, DiaObjectTypePackage).map(processPackage) |> liftFirstError
    packages.map { p => f.copy(packages = p)}
  }

  def assertNodeName(n: Node, expected: String) {
    val nodeLabel = n.label
    if (nodeLabel != expected) throw new RuntimeException(s"Node label is not an '$expected', but '$nodeLabel'.\n$n")
  }

  def assertAttributeType(n: Node, expected: String) {
    val attributeName = n \@ DiaAttributeType
    if (attributeName != expected) throw new RuntimeException(s"Attribute type is not an '$expected', but '$attributeName' (label='${n.label}', text='${n.text}').\n$n")
  }

  def assertNodeObjectAndTypeAttribute(n: Node, expected: String) {
    assertNodeName(n, DiaNodeTypeObject)
    assertAttributeType(n, expected)
  }

  def extractAttributeName(n: Node): String = extractDiaAttributeStringAndStrip(n, DiaAttributeName)

  def processAttribute(n: Node): DiaAttribute = {
    assertAttributeType(n, DiaCompositeTypeUMLAttribute)
    DiaAttribute(
      extractAttributeName(n),
      extractDiaAttributeStringAndStrip(n, DiaAttributeType) |> convertTypeWithoutClassExistenceChecks,
      extractVisibility(n)
    )
  }

  def processParameter(n: Node): DiaOperationParameter = {
    assertAttributeType(n, DiaCompositeTypeUMLParameter)
    DiaOperationParameter(
      extractAttributeName(n),
      extractDiaAttributeStringAndStrip(n, DiaAttributeType) |> convertTypeWithoutClassExistenceChecks
    )
  }

  def processOperation(n: Node): DiaOperationDescriptor = {
    assertAttributeType(n, DiaCompositeTypeUMLOperation)
    DiaOperationDescriptor(
      extractAttributeName(n),
      extractVisibility(n),
      (extractDiaAttributeByName(n, DiaAttributeParameters) \ DiaNodeTypeComposite).map(processParameter),
      extractDiaAttributeStringAndStrip(n, DiaAttributeType) |> convertTypeWithoutClassExistenceChecks
    )
  }

  def processClass(n: Node): \/[String, DiaClass] = {
    assertNodeObjectAndTypeAttribute(n, DiaObjectTypeClass)
    wrapErrorToJunction {
      val stereotype = extractDiaAttributeStringAndStrip(n, DiaAttributeStereotype)
      val classType = stereotype match {
        case "interface" | "trait" => Trait
        case "enum" | "enumeration" => Enumeration
        case "singleton" | "object" => Object
        case "" => Class
        case s =>
          Log.printInfo(s"Ignoring not recognized class stereotype '$s'.")
          Class
      }

      val attributes = (extractDiaAttributesMatchingName(n, DiaAttributeAttributes) \ DiaNodeTypeComposite).map(processAttribute)
      val operations = (extractDiaAttributesMatchingName(n, DiaAttributeOperations) \ DiaNodeTypeComposite).map(processOperation)

      DiaClass(
        extractAttributeName(n) |> DiaFile.createUncheckedClassRef,
        extractGeometry(n),
        None,
        Seq(),
        n \@ DiaAttributeId,
        attributes,
        operations,
        classType
      )
    }
  }

  def getPackageForClass(f: DiaFile, c: DiaClass): Option[DiaPackage] = {
    val inPackages = f.packages.filter(p => p.geometry.contains(c.geometry))
    // TODO: support for nested packages
    // sort by "contains" and return either most inner or path of packages
    inPackages.headOption
  }

  def assignPackages(f: DiaFile): DiaFile = {
    val newClasses = f.classes map { c =>
      c.copy(ref = c.ref.copy(inPackage = getPackageForClass(f, c).map(_.name).getOrElse("")))
    }
    assert(newClasses.size == f.classes.size)
    f.copy(classes = newClasses)
  }

  def createIdToClassMapping(f: DiaFile): DiaFile = f.copy(idToClass = f.classes.map { c => c.id -> c}.toMap)

  def semiProcessClasses(e: Elem, f: DiaFile): \/[String, DiaFile] =
    for {
      parsedClasses <- extractObjectsByType(e, DiaObjectTypeClass).map(processClass) |> liftFirstError
    } yield f.copy(classes = parsedClasses) |>
      assignPackages |> createIdToClassMapping

  def parseConnections(n: Node): (String, String) = {
    assertNodeName(n, DiaNodeTypeConnections)
    def getTargetOfConnection(handle: Int): String = ((n \ DiaNodeTypeConnection) filter (_ \@ "handle" == handle.toString)) \@ "to"
    (getTargetOfConnection(1), getTargetOfConnection(0)) // from -> to
  }

  def parsedConnectionIdsToOneWayConnection(connectionsIds: (String, String), cType: DiaOneWayConnectionType): DiaOneWayConnection =
    DiaOneWayConnection(connectionsIds._1, connectionsIds._2, cType)

  def parseGeneralization(n: Node): Option[DiaOneWayConnection] = {
    assertNodeObjectAndTypeAttribute(n, DiaObjectTypeGeneralization)
    parseConnections((n \ DiaNodeTypeConnections).head) |> {parsedConnectionIdsToOneWayConnection(_, DiaGeneralizationType)} |> Some.apply
  }

  def processGeneralization(i: OneWayConnectionProcessorData): DiaFile =
    i.f.copy(classes = i.f.classes.map { c =>
      val conn = i.c
      if (c.id == conn.fromId) {
        val toClassRef = i.f.idToClass.get(conn.toId) match {
          case Some(cl) => cl.ref
          case None => throw new RuntimeException(s"Unable to find target class ${formatClassId(conn.toId, i.f)} of ${formatConnection(conn, i.f)}")
        }
        if (c.extendsFrom.nonEmpty) throw new RuntimeException(s"Multiple generalizations for class ${c.ref} (${c.extendsFrom} -> ${toClassRef.fullName}).")
        c.copy(extendsFrom = toClassRef.some)
      } else c
    })

  def parseRealizes(n: Node): Option[DiaOneWayConnection] = {
    assertNodeObjectAndTypeAttribute(n, DiaObjectTypeRealizes)
    val conn = parseConnections((n \ DiaNodeTypeConnections).head)
    val cType: Option[DiaOneWayConnectionType] = extractDiaAttributeStringAndStrip(n, DiaAttributeStereotype) match {
      case "" => DiaImplementsType.some
      case "mixin" => DiaMixinType.some
      case "hasA" | "companionOf" => DiaCompanionOfType.some
      case s =>
        Log.printInfo(s"Skipping unknown realizes connection: '$s'.")
        None
    }
    cType.map { ct => conn |> {parsedConnectionIdsToOneWayConnection(_, ct)}}
  }

  def processRealizes(i: OneWayConnectionProcessorData): DiaFile =
    i.f.copy(classes = i.f.classes.map { c =>
      if (c.id == i.c.fromId) {
        val conn = i.c
        val toClassRef = i.f.idToClass(conn.toId).ref
        conn.cType match {
          case DiaImplementsType | DiaMixinType =>
            if (c.extendsFrom.isEmpty) c.copy(extendsFrom = toClassRef.some)
            else c.copy(mixins = c.mixins :+ toClassRef)
          case DiaCompanionOfType => c
          case t => throw new RuntimeException(s"Invalid connection type $t.")
        }
      } else c
    })

  case class OneWayConnectionProcessorData(f: DiaFile, c: DiaOneWayConnection, fromIdToConn: Map[String, DiaOneWayConnection], toIdToConn: Map[String, DiaOneWayConnection])

  def formatClassId(id: String, f: DiaFile): String = s"${f.idToClass.get(id).map(_.ref.fullName).getOrElse("<Class not found>")}($id)"

  def formatConnection(c: DiaOneWayConnection, f: DiaFile): String = c.cType + ": " + formatClassId(c.fromId, f) + " -> " + formatClassId(c.toId, f)

  def formatConnectionsSeq(s: Seq[DiaOneWayConnection], f: DiaFile): String = s.map(formatConnection(_, f)).mkString(", ")

  def processOneWayConnection(n: Node, f: DiaFile, objType: String, extractor: (Node) => Option[DiaOneWayConnection], processor: (OneWayConnectionProcessorData) => DiaFile): \/[String, DiaFile] =
    wrapErrorToJunction {
      val nodes = extractObjectsByType(n, objType)
      val connections = nodes.map(extractor).filter(_.isDefined).map(_.get)
      val fromIdToConn = connections.map { c => c.fromId -> c}.toMap
      val toIdToConn = connections.map { c => c.toId -> c}.toMap
      Log.printTrace(s"node count: ${nodes.size}, connections: ${formatConnectionsSeq(connections, f)}")
      connections.foldLeft(f) { (acc, item) => processor(OneWayConnectionProcessorData(acc, item, fromIdToConn, toIdToConn))}
    }

  def processOneWayConnections(e: Elem, f: DiaFile): \/[String, DiaFile] =
    for {
      a <- processOneWayConnection(e, f, DiaObjectTypeGeneralization, parseGeneralization, processGeneralization)
      b <- processOneWayConnection(e, a, DiaObjectTypeRealizes, parseRealizes, processRealizes)
    } yield b
}
