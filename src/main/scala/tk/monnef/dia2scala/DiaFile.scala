package tk.monnef.dia2scala

case class DiaFile(packages: Seq[DiaPackage], classes: Seq[DiaClass])

case class DiaPackage(name: String, geometry: DiaGeometry)

case class DiaClass(name: String, geometry: DiaGeometry, inPackage: String, extendsFrom: String, mixins: Seq[String], id: String)

case class DiaOperatorDescriptor(name: String, visibility: DiaVisibility, parameters: Seq[DiaOperatorParameter])

case class DiaOperatorParameter(name: String, pType: Option[String])

case class DiaAttribute(name: String, aType: Option[String], visibility: DiaVisibility)

case class DiaGeometry(x: Double, y: Double, width: Double, height: Double)

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


sealed class DiaVisibility

case object DiaPrivate extends DiaVisibility

case object DiaProtected extends DiaVisibility

case object DiaPublic extends DiaVisibility

case object DiaImplementation extends DiaVisibility
