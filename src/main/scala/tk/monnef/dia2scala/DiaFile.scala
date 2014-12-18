package tk.monnef.dia2scala

import tk.monnef.dia2scala.DiaClassType.DiaClassType
import tk.monnef.dia2scala.DiaVisibility.DiaVisibility

case class DiaFile(packages: Seq[DiaPackage], classes: Seq[DiaClass], idToClass: Map[String, DiaClass])

object DiaFile {
  def apply(): DiaFile = DiaFile(Seq(), Seq(), Map())
}

case class DiaPackage(name: String, geometry: DiaGeometry)

case class DiaClass(name: String, geometry: DiaGeometry, inPackage: String, extendsFrom: String, mixins: Seq[String], id: String, attributes: Seq[DiaAttribute], operations: Seq[DiaOperationDescriptor], classType: DiaClassType)

case class DiaOperationDescriptor(name: String, visibility: DiaVisibility, parameters: Seq[DiaOperationParameter], oType: Option[String])

case class DiaOperationParameter(name: String, pType: Option[String])

case class DiaAttribute(name: String, aType: Option[String], visibility: DiaVisibility)

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
