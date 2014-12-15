package tk.monnef.dia2scala

import tk.monnef.dia2scala.DiaVisibility.DiaVisibility

case class DiaFile(packages: Seq[DiaPackage], classes: Seq[DiaClass])

object DiaFile {
  def apply(): DiaFile = DiaFile(Seq(), Seq())
}

case class DiaPackage(name: String, geometry: DiaGeometry)

case class DiaClass(name: String, geometry: DiaGeometry, inPackage: String, extendsFrom: String, mixins: Seq[String], id: String, attributes: Seq[DiaAttribute], operations: Seq[DiaOperationDescriptor])

case class DiaOperationDescriptor(name: String, visibility: DiaVisibility, parameters: Seq[DiaOperationParameter])

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
}
