package tk.monnef.dia2scala

import java.io.{File, FileInputStream, InputStream, InputStreamReader}
import java.util.zip.GZIPInputStream

import tk.monnef.dia2scala.DiaClassRefBase.fromStringUnchecked
import tk.monnef.dia2scala.DiaClassType._
import tk.monnef.dia2scala.DiaVisibility.DiaVisibility
import tk.monnef.dia2scala.Utils._

import scala.util.Try

object XmlParser {

  import tk.monnef.dia2scala.XmlParserHelper._

  import scalaz.Scalaz._
  import scalaz._

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
          d <- processErrors(c)
        } yield d
    }
  }
}

object XmlParserHelper {

  import scala.xml.{Elem, Node, NodeSeq}
  import scalaz.Scalaz.ToIdOps
  import scalaz._
  import scalaz.syntax.either._
  import scalaz.syntax.std.option._

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

  val AttributeStereotypesPattern = "^<<([a-zA-Z ,]+)>>\\s*([a-zA-Z0-9]+)\\s*$".r
  val AttrStereoVal = "val"
  val AttrStereoVar = "var"
  val ValidAttrStereos = Seq(AttrStereoVal, AttrStereoVar)

  def ensureAttributeObeysClassStereotypes(attrStereo: String, isClassVal: Option[Boolean], attrName: String) {
    if (isClassVal.isDefined) {
      val attrIsVal = attrStereo == AttrStereoVal
      if (isClassVal.get != attrIsVal) {
        throw new RuntimeException(s"'$attrName': Attribute and class stereotype aren't compatible.")
      }
    }
  }

  def processAttribute(n: Node, isClassVal: Option[Boolean]): DiaAttribute = {
    assertAttributeType(n, DiaCompositeTypeUMLAttribute)
    val nameField = extractAttributeName(n)
    val (name: String, isVal: Boolean) = {
      if (nameField.startsWith("<<")) {
        AttributeStereotypesPattern.findFirstMatchIn(nameField) match {
          case Some(rMatch) =>
            val stereo = rMatch.group(1)
            val name = rMatch.group(2)
            if (!ValidAttrStereos.contains(stereo)) {
              Log.printInfo(s"Skipping unknown attribute stereotype '$stereo'.")
              (name, true)
            } else {
              ensureAttributeObeysClassStereotypes(stereo, isClassVal, name)
              (name, stereo != AttrStereoVar)
            }
          case None => throw new RuntimeException(s"Failed to parse an attribute with stereotype: '$nameField'")
        }
      } else (nameField, true)
    }

    DiaAttribute(
      name,
      extractDiaAttributeStringAndStrip(n, DiaAttributeType) |> fromStringUnchecked,
      extractVisibility(n),
      isVal,
      extractDiaAttributeStringAndStrip(n, DiaAttributeValue) |> wrapNonEmptyStringToSome
    )
  }

  def processParameter(n: Node): DiaOperationParameter = {
    assertAttributeType(n, DiaCompositeTypeUMLParameter)
    DiaOperationParameter(
      extractAttributeName(n),
      extractDiaAttributeStringAndStrip(n, DiaAttributeType) |> fromStringUnchecked
    )
  }

  def processOperation(n: Node): DiaOperationDescriptor = {
    assertAttributeType(n, DiaCompositeTypeUMLOperation)
    DiaOperationDescriptor(
      extractAttributeName(n),
      extractVisibility(n),
      (extractDiaAttributeByName(n, DiaAttributeParameters) \ DiaNodeTypeComposite).map(processParameter),
      extractDiaAttributeStringAndStrip(n, DiaAttributeType) |> fromStringUnchecked
    )
  }

  val ClassStereoInterface = "interface"
  val ClassStereoTrait = "trait"
  val ClassStereoEnumeration = "enumeration"
  val ClassStereoEnum = "enum"
  val ClassStereoSingleton = "singleton"
  val ClassStereoObject = "object"
  val ClassStereoMutable = "mutable"
  val ClassStereoImmutable = "immutable"
  val ValidClassStereos = Seq(ClassStereoInterface, ClassStereoTrait, ClassStereoEnum, ClassStereoEnumeration, ClassStereoSingleton, ClassStereoObject, ClassStereoMutable, ClassStereoImmutable)

  def processClass(n: Node): \/[String, DiaClass] = {
    assertNodeObjectAndTypeAttribute(n, DiaObjectTypeClass)
    wrapErrorToJunction {
      val stereotypes = extractDiaAttributeStringAndStrip(n, DiaAttributeStereotype).split("[,;] *").toSeq
      val classTypes = stereotypes.flatMap {
        case ClassStereoInterface | ClassStereoTrait => Seq(Trait)
        case ClassStereoEnum | ClassStereoEnumeration => Seq(Enumeration)
        case ClassStereoSingleton | ClassStereoObject => Seq(Object)
        case "" => Seq(Class)
        case s =>
          if (!ValidClassStereos.contains(s)) Log.printInfo(s"Ignoring not recognized class stereotype '$s'.")
          Seq()
      }
      if (classTypes.size > 1) throw new RuntimeException(s"Class object holds multiple contradicting stereotypes - $stereotypes")
      val classType = if (classTypes.nonEmpty) classTypes.head else Class

      val isMutable = stereotypes.contains(ClassStereoMutable)
      val isImmutable = stereotypes.contains(ClassStereoImmutable)

      val classRef = extractAttributeName(n) |> DiaClassRefBase.createUncheckedUserClassRef

      if (isMutable && isImmutable) throw new RuntimeException(s"Class '${classRef.name}' is mutable AND immutable, this cannot happen in our universe.")
      val isVal = if (isMutable) Some(false) else if (isImmutable) Some(true) else None

      val attributesRaw = (extractDiaAttributesMatchingName(n, DiaAttributeAttributes) \ DiaNodeTypeComposite).map(processAttribute(_, isVal))
      val attributes = attributesRaw.map { a => a.copy(isVal = if (isMutable) false else if (isImmutable) true else a.isVal)}
      val operations = (extractDiaAttributesMatchingName(n, DiaAttributeOperations) \ DiaNodeTypeComposite).map(processOperation)

      DiaClass(
        classRef,
        extractGeometry(n),
        None,
        Seq(),
        n \@ DiaAttributeId,
        attributes,
        operations,
        classType,
        isMutable,
        isImmutable
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

  def processClassRefInSamePackage(f: DiaFile): DiaFile = f.copy(classes = {
    val classToPackage = f.classes.map { c => c.ref.name -> c.ref.inPackage}.toMap

    def handleRef(r: DiaUserClassRef): DiaUserClassRef =
      if (r.inPackage.nonEmpty) r
      else classToPackage.get(r.name) match {
        case Some(pck) => r.copy(inPackage = pck)
        case None => r
      }

    def handleAttribute(a: DiaAttribute): DiaAttribute = a.copy(aType = handleOptClassRef(a.aType))

    def handleOptClassRef(r: Option[DiaClassRefBase]): Option[DiaClassRefBase] = r match {
      case Some(userClass: DiaUserClassRef) => handleRef(userClass).some
      case _ => r
    }

    def handleOperation(o: DiaOperationDescriptor): DiaOperationDescriptor = o.copy(
      oType = handleOptClassRef(o.oType),
      parameters = o.parameters.map { p => p.copy(pType = handleOptClassRef(p.pType))}
    )

    f.classes.map {
      c => c.copy(
        attributes = c.attributes.map(handleAttribute),
        operations = c.operations.map(handleOperation)
      )
    }
  })

  def semiProcessClasses(e: Elem, f: DiaFile): \/[String, DiaFile] =
    for {
      parsedClasses <- extractObjectsByType(e, DiaObjectTypeClass).map(processClass) |> liftFirstError
    } yield f.copy(classes = parsedClasses) |>
      assignPackages |> createIdToClassMapping |> processClassRefInSamePackage

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

  def processErrors(f: DiaFile): \/[String, DiaFile] = {
    val errs = f.validationErrors()
    if (errs.isEmpty) f.right
    else errs.mkString("\n").left
  }

}
