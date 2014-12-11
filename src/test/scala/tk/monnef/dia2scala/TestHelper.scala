package tk.monnef.dia2scala

object TestHelper {
  def genPath(filename: String) = "src/test/resources/" + filename

  def genDiagramPath(filename: String) = genPath("diagrams/" + filename) + ".dia"

  def currentPath() = new java.io.File(".").getAbsolutePath
}
