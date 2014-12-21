package tk.monnef.dia2scala

import tk.monnef.dia2scala.DiaClassType.DiaClassType
import tk.monnef.dia2scala.DiaVisibility.DiaVisibility

import scalaz.syntax.either._
import scalaz.syntax.optional._
import scalaz.syntax.std.all._
import scalaz.Scalaz.ToIdOps
import scalaz.{\/-, -\/, \/}

case class DiaFile(packages: Seq[DiaPackage], classes: Seq[DiaClass], idToClass: Map[String, DiaClass]) {

  import DiaFile._

  def validationErrors(): Seq[String] = {
    classes.flatMap(_.validationErrors()) ++
      validatePackages(this) ++
      validateClassReferences(this)
  }

  def findClass(fullName: String): Option[DiaClass] = {
    val (pck, cName) = fullName.split("\\.") |> { a => (a.init.mkString("."), a.last)}
    findClass(pck, cName)
  }

  def findClass(inPackage: String, name: String): Option[DiaClass] = {
    val targetRef = DiaClassRef(name, inPackage)
    classes.find(c => c.ref == targetRef)
  }

  def findClass(ref: DiaClassRef): Option[DiaClass] = {
    findClass(ref.inPackage, ref.name)
  }

  def classExists(ref: DiaClassRef): Boolean = findClass(ref).isDefined

  def convertType(i: String): Option[\/[String, DiaClassRef]] = {
    if (isScalaClass(i)) i.left.some
    else {
      findClass(i) match {
        case Some(c) => c.ref.right.some
        case None => None
      }
    }
  }

  def isScalaClass(n: String): Boolean = ScalaClasses.contains(n)
}

object DiaFile {
  def apply(): DiaFile = DiaFile(Seq(), Seq(), Map())

  def generateSeqIfTrue[T](cond: Boolean, msg: T): Seq[T] = if (cond) Seq(msg) else Seq()

  private final val emptySeqSeqString = Seq[Seq[String]]()

  def validatePackages(f: DiaFile): Seq[String] = {
    (
      ({
        val d = f.packages diff f.packages.distinct
        if (d.nonEmpty) Seq(Seq(s"Duplicate packages detected (${d.mkString(", ")})."))
        else Seq()
      }: Seq[Seq[String]]) ++ {
        val ep = f.packages.filter(p => !f.classes.exists(c => c.ref.inPackage == p.name))
        if (ep.nonEmpty) Seq(Seq(s"Empty packages detected (${ep.mkString(", ")})."))
        else Seq()
      }: Seq[Seq[String]]
      ).flatten
  }

  def validateClassReferences(f: DiaFile): Seq[String] = {
    (
      ({
        val ur = f.classes.filter(c => c.extendsFrom.isDefined && !f.classExists(c.extendsFrom.get))
        if (ur.nonEmpty) Seq(Seq(s"Found class(es) extending non-existent classes (${ur.map(c => c.ref.name + " extending " + c.extendsFrom.get.name).mkString(", ")}})."))
        else Seq()
      }: Seq[Seq[String]]) ++ ({
        val mt = f.classes.map(c => {
          val missingTraits = c.mixins.filter(m => !f.classExists(m))
          c -> missingTraits
        }).filter(_._2.nonEmpty)
        if (mt.nonEmpty) Seq(Seq(s"Found class(es) mixing in non-existent traits (${mt.map { case (c, ms) => c.ref.name + " using " + ms.map(_.name).mkString(", ")}.mkString("; ")}})."))
        else emptySeqSeqString
      }: Seq[Seq[String]]) ++ ({
        f.classes.map(c => {
          c.attributes.filter(a => a.aType.nonEmpty).flatMap(a => {
            a.aType.get match {
              case -\/(sc) => if (f.isScalaClass(sc)) Seq() else Seq(s"Type $sc of attribute ${a.name} in ${c.ref.name} is not a Scala class.")
              case \/-(ref) => if (f.classExists(ref)) Seq() else Seq(s"Class ${ref.name} of attribute ${a.name} in ${c.ref.name} not found.")
            }
          })
        })
      }: Seq[Seq[String]]) ++ ({
        for {
          c <- f.classes
          o <- c.operations
          if o.oType.isDefined
          t = o.oType.get
        } yield t match {
          case -\/(sc) => if (f.isScalaClass(sc)) Seq() else Seq(s"Return type $sc of operations ${o.name} in ${c.ref.name} is not a Scala class.")
          case \/-(ref) => if (f.classExists(ref)) Seq() else Seq(s"Return type ${ref.name} of operation ${o.name} in ${c.ref.name} not found.")
        }
      }: Seq[Seq[String]]) ++ {
        (for {
          c <- f.classes
          o <- c.operations
          p <- o.parameters
          if p.pType.isDefined
          t = p.pType.get
        } yield t match {
            case -\/(sc) => if (f.isScalaClass(sc)) Seq() else Seq(s"Type $sc of parameter ${p.name} of operations ${o.name} in ${c.ref.name} is not a Scala class.")
            case \/-(ref) => if (f.classExists(ref)) Seq() else Seq(s"Type ${ref.name} of parameter ${p.name} of operation ${o.name} in ${c.ref.name} not found.")
          }): Seq[Seq[String]]
      }
      ).flatten
  }

  final val ScalaClasses = Seq("Seq", "Int", "String", "Double", "Float", "Map", "List", "Set", "Option", "Either", "Char", "Boolean", "Byte", "Short", "Long", "Any", "AnyVal", "AnyRef")
}

trait Checkable {
  def validationErrors(): Seq[String]
}

case class DiaPackage(name: String, geometry: DiaGeometry) extends Checkable {
  override def validationErrors(): Seq[String] = Seq()
}

case class DiaClassRef(name: String, inPackage: String) {
  lazy val fullName = s"$inPackage.$name"
}

case class DiaClass(ref: DiaClassRef, geometry: DiaGeometry, extendsFrom: Option[DiaClassRef], mixins: Seq[DiaClassRef], id: String, attributes: Seq[DiaAttribute], operations: Seq[DiaOperationDescriptor], classType: DiaClassType) extends Checkable {
  override def validationErrors(): Seq[String] = {
    ({
      if (ref.name.trim.isEmpty) Seq(Seq(s"Class with empty name ($id)."))
      else Seq()
    }: Seq[Seq[String]]).flatten
  }
}

case class DiaOperationDescriptor(name: String, visibility: DiaVisibility, parameters: Seq[DiaOperationParameter], oType: Option[\/[String, DiaClassRef]])

case class DiaOperationParameter(name: String, pType: Option[\/[String, DiaClassRef]])

case class DiaAttribute(name: String, aType: Option[\/[String, DiaClassRef]], visibility: DiaVisibility)

case class DiaGeometry(x: Double, y: Double, width: Double, height: Double) {
  def contains(other: DiaGeometry): Boolean = contains(other.x, other.y, other.width, other.height)

  def contains(ox: Double, oy: Double, oWidth: Double, oHeight: Double): Boolean =
    ox > x &&
      oy > y &&
      ox + oWidth < x + width &&
      oy + oHeight < y + height
}

case class DiaOneWayConnection(fromId: String, toId: String, cType: DiaOneWayConnectionType)

case class DiaAssociation(from: DiaAssociationNode, to: DiaAssociationNode)

case class DiaAssociationNode(id: String, arity: DiaAssociationArity, attr: DiaAttribute)


sealed class DiaAssociationArity

case object DiaArityNone extends DiaAssociationArity

case object DiaArityOne extends DiaAssociationArity

case object DiaArityMultiple extends DiaAssociationArity


sealed class DiaOneWayConnectionType

case object DiaGeneralizationType extends DiaOneWayConnectionType

case object DiaImplementsType extends DiaOneWayConnectionType

case object DiaMixinType extends DiaOneWayConnectionType

case object DiaCompanionOfType extends DiaOneWayConnectionType

object DiaVisibility extends Enumeration {
  type DiaVisibility = Value
  val Public, Private, Protected, Implementation = Value

  final val dvToCode = Map(
    Public -> "public",
    Private -> "private",
    Protected -> "protected",
    Implementation -> "???"
  )

  implicit class DiaVisibilityPimps(val dv: DiaVisibility) {
    def code: String = dvToCode(dv)
  }

}

object DiaClassType extends Enumeration {
  type DiaClassType = Value
  val Class, Enumeration, Trait, Object = Value
}
