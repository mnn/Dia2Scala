package tk.monnef.dia2scala

case class DiaFile(packages: Seq[DiaPackage], classes: Seq[DiaClass])

case class DiaPackage(name: String, geometry: DiaGeometry)

case class DiaClass(name: String, geometry: DiaGeometry)

case class DiaGeometry(x: Int, y: Int, width: Int, height: Int)
