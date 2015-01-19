package tk.monnef.dia2scala

case class ImportTable(nameToFullName: Map[String, String]) {
  val fullNameToName = nameToFullName.toSeq.map(_.swap).toMap

  def fullNameForClass(name: String): Option[String] = nameToFullName.get(name)

  def containsFullName(fullName: String): Boolean = fullNameToName.contains(fullName)
}

object ImportTable {
  final val default = ImportTable(Map(
    "PriorityQueue" -> "scala.collection.mutable.PriorityQueue",
    "Queue" -> "scala.collection.immutable.Queue"
  ))

  final val empty = ImportTable(Map())
}
