package tk.monnef.dia2scala

import tk.monnef.dia2scala.DiaVisibility._
import scalaz._
import Scalaz._
import Utils._

object CodeEmitter {

  import CodeEmitterHelper._

  def emit(file: DiaFile): \/[String, EmittedCode] = {
    EmittedCode(
      emitClasses(file.classes)
    ).right
  }
}

case class EmittedCode(classes: Seq[EmittedClass])

case class EmittedClass(name: String, inPackage: String, code: String, inFile: String)

object CodeEmitterHelper {
  def emitClass(c: DiaClass): EmittedClass = {
    val indent = "  "
    def genClass = s"class ${c.name}"
    def genExtends = !c.extendsFrom.isEmpty ? s" extends ${c.extendsFrom}" | ""
    def genMixins = if (c.mixins.isEmpty) "" else c.mixins.mkString(" with ", " with ", "")
    def genVisibility(v: DiaVisibility) = if (v == DiaVisibility.Public) "" else v.code + " "
    def genType(t: Option[String]): String = t.map(": " + _) | ""
    def genAttributes: Seq[String] = c.attributes.map { a: DiaAttribute =>
      indent +
        genVisibility(a.visibility) +
        "var " +
        a.name +
        genType(a.aType) +
        " = _" +
        ""
    }
    def genParameter(p: DiaOperationParameter): String = p.name + genType(p.pType)
    def genOperations: Seq[String] = c.operations.map { o: DiaOperationDescriptor =>
      indent +
        genVisibility(o.visibility) +
        "def " +
        o.name +
        "(" +
        o.parameters.map(genParameter).mkString(", ") +
        ")" +
        o.oType.map(": " + _ + " = ???").getOrElse("{ ??? }") +
        ""
    }

    val dnl = "\n\n"
    val nl = "\n"

    val code = (new CodeBuilder).
      addLine(genClass + genExtends + genMixins + " {").
      addLineSeq(genAttributes).
      addLine().
      setMaximumOfSuccessiveBlankLines(2).
      addLineSeq(genOperations, doubleNewLines = true).
      setMaximumOfSuccessiveBlankLines(1).
      addLine("}").
      convertToString

    EmittedClass(
      c.name,
      c.inPackage,
      code,
      c.name.capitalizeFirst + ".scala"
    )
  }

  def emitClasses(cs: Seq[DiaClass]): Seq[EmittedClass] = cs map emitClass
}

class CodeBuilder {

  import CodeBuilder._

  var maximumOfSuccessiveBlankLines = 1

  private var _code = Seq[String]()
  private var _successiveBlankLines = 0

  def code: Seq[String] = _code

  def convertToString = {
    val r = code.mkString("", "\n", "\n")

    if (isBlankLine(r) && maximumOfSuccessiveBlankLines == 0) ""
    else r
  }

  def computeEndingBlankLines = _code.size - _code.dropWhileRight(isBlankLine).size

  def setMaximumOfSuccessiveBlankLines(max: Int): CodeBuilder = {
    maximumOfSuccessiveBlankLines = max
    this
  }

  def addString(multiLines: String, doubleNewLines: Boolean = false): CodeBuilder = {
    multiLines.split("\n") |> {addLineSeq(_, doubleNewLines)}
    this
  }

  def addLineSeq(lines: Seq[String], doubleNewLines: Boolean = false): CodeBuilder = {
    lines.foreach { i =>
      addLine(i)
      if (doubleNewLines) addLine()
    }
    this
  }

  def addLine(line: String): CodeBuilder = {
    def addLineImpl() {
      def appendCode() { _code :+= line }
      if (isBlankLine(line)) {
        if (maximumOfSuccessiveBlankLines > _successiveBlankLines) {
          _successiveBlankLines += 1
          appendCode()
        }
      } else {
        _successiveBlankLines = 0
        appendCode()
      }
    }

    if (line.contains("\n")) throw new RuntimeException(s"Line contains multiple lines:\n$line")
    addLineImpl()
    this
  }

  def addLine(): CodeBuilder = {
    addLine("")
  }

  def addDoubleLine(): CodeBuilder = {
    addLine()
    addLine()
  }
}

object CodeBuilder {
  def isBlankLine(s: String): Boolean = s.trim.isEmpty
}
