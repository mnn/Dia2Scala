package tk.monnef.dia2scala

import tk.monnef.dia2scala.DiaVisibility._
import scalaz._
import Scalaz._
import Utils._

object CodeEmitter {

  import CodeEmitterHelper._

  def emit(file: DiaFile): \/[String, EmittedCode] = {
    Log.printDebug(s"CodeEmitter.emit:")
    val r = EmittedCode(
      emitParts(file.entities),
      file.entities.flatMap(c => c.extendsFrom.map(e => (c.ref.emitCodeWithFullName(), e.emitCodeWithFullName())))
    ).right
    Log.printTrace(s"res:\n${r.toString}")
    r
  }
}

case class EmittedCode(parts: Seq[EmittedParts], dependencies: Seq[(String, String)])

case class EmittedParts(name: String, inPackage: String, code: String, inFile: String, requiredClasses: Seq[String]) {
  lazy val fullName = (if (inPackage.isEmpty) "" else inPackage + ".") + name
}

object CodeEmitterHelper {
  def emitClass(c: DiaClass): EmittedParts = {
    val indent = "  "
    def genClass = (c.classType match {
      case DiaClassType.Class => "class"
      case DiaClassType.Object | DiaClassType.Enumeration => "object"
      case DiaClassType.Trait => "trait"
    }) + s" ${c.ref.name}"

    def genExtends = (if (c.classType == DiaClassType.Enumeration) DiaClassRefBase.fromStringUnchecked("Enumeration") else c.extendsFrom).
      map(ef => s" extends ${ef.emitCode()}").getOrElse("")

    def genMixins = if (c.mixins.isEmpty) "" else c.mixins.map(_.emitCode()).mkString(" with ", " with ", "")

    def genVisibility(v: DiaVisibility) = if (v == DiaVisibility.Public) "" else v.code + " "

    def genTypePart(t: DiaClassRefBase): String = ": " + t.emitCode()

    def genOptTypePart(t: Option[DiaClassRefBase]): String = t.map(genTypePart).getOrElse("")

    def genAttributes: Seq[String] = c.attributes.map { a: DiaAttribute =>
      indent +
        genVisibility(a.visibility) +
        (if (a.isVal) "val " else "var ") +
        a.name +
        genOptTypePart(a.aType) +
        " = _" +
        ""
    }

    def genParameter(p: DiaOperationParameter): String = p.name + genOptTypePart(p.pType)

    def genOperations: Seq[String] = c.operations.map { o: DiaOperationDescriptor =>
      indent +
        genVisibility(o.visibility) +
        "def " +
        o.name +
        "(" +
        o.parameters.map(genParameter).mkString(", ") +
        ")" +
        o.oType.map(t => genTypePart(t) + " = ???").getOrElse("{ ??? }") +
        ""
    }

    def genImportsForClassRef(cr: DiaClassRefBase): Seq[String] = {
      cr match {
        case x: DiaUserClassRef => Seq(x.fullName)
        case x: DiaGenericClassRef => genImportsForClassRef(x.base) ++ x.params.flatMap(genImportsForClassRef)
        case x: DiaTupleClassRef => x.params.flatMap(genImportsForClassRef)
        case _ => Seq()
      }
    }

    def genImportsForClassRefOpt(cr: Option[DiaClassRefBase]): Seq[String] = cr.map(genImportsForClassRef).getOrElse(Seq())

    def genImportsForMixins: Seq[String] =
      (if (c.extendsFrom.isEmpty) Seq() else Seq(c.extendsFrom.get.emitCodeWithFullName())) ++
        c.mixins.map(_.emitCodeWithFullName())

    def genImportsForAttributes: Seq[String] = c.attributes.flatMap {_.aType |> genImportsForClassRefOpt}

    def genImportsForOperations: Seq[String] = c.operations.flatMap { o =>
      (o.oType |> genImportsForClassRefOpt) ++
        o.parameters.flatMap(_.pType |> genImportsForClassRefOpt)
    }

    def genImportsForCompanionObject: Seq[String] = if (c.hasCompanionObject) Seq(c.ref.fullName + "._") else Seq()

    def genImports: Seq[String] = {
      genImportsForMixins ++
        genImportsForAttributes ++
        genImportsForOperations ++
        genImportsForCompanionObject
    }

    def emitClassHeader(cb: CodeBuilder): CodeBuilder = {
      cb.
        addLine(genClass + genExtends + genMixins + " {")
    }

    def emitClassBody(cb: CodeBuilder): CodeBuilder = {
      cb |> { cb: CodeBuilder => if (c.classType == DiaClassType.Enumeration) {
        cb.
          addLine(indent + s"type ${c.ref.emitCode()} = Value").
          addLine(indent + "val " + c.attributes.map(_.name).mkString(", ") + " = Value")
      } else {
        cb.
          addLineSeq(genAttributes)
      }
      } |> { cb: CodeBuilder =>
        cb.
          addLine().
          setMaximumOfSuccessiveBlankLines(2).
          addLineSeq(genOperations, doubleNewLines = true).
          setMaximumOfSuccessiveBlankLines(1)
      }
    }

    def emitClassEnding(cb: CodeBuilder): CodeBuilder = {
      cb.
        setMaximumOfSuccessiveBlankLines(1).
        addLine("}")
    }

    val code = new CodeBuilder |>
      emitClassHeader |> emitClassBody |> emitClassEnding |> {_.convertToString}

    EmittedParts(
      c.ref.name,
      c.ref.inPackage,
      code,
      c.ref.name.capitalizeFirst + ".scala",
      genImports
    )
  }

  def emitParts(cs: Seq[DiaClass]): Seq[EmittedParts] = cs map emitClass
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
