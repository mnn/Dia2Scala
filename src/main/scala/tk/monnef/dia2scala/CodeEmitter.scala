package tk.monnef.dia2scala

import tk.monnef.dia2scala.DiaVisibility._
import scalaz._
import Scalaz._
import Utils._

// TODO: import table, e.g. PriorityQueue -> add import "scala.collection.mutable.PriorityQueue"
// TODO: override modifiers?
object CodeEmitter {

  import CodeEmitterHelper._

  def emit(file: DiaFile): \/[String, EmittedCode] = {
    Log.printDebug(s"CodeEmitter.emit:")
    val r = EmittedCode(
      emitParts(file),
      generateDependencies(file)
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
  def generateDependencies(file: DiaFile): Seq[(String, String)] = {
    file.entities.flatMap(c => generateExtendsFromField(c).map(e => (c.ref.emitCodeWithFullName(), e.emitCodeWithFullName())))
  }

  def generateFieldsExtendsFromAndMixins(c: DiaEntity): (Option[DiaClassRefBase], Seq[DiaClassRefBase]) = c.extendsFrom match {
    case Some(e) => (e.some, c.mixins)
    case None => c.mixins.headOption match {
      case Some(head) => (head.some, c.mixins.tail)
      case None => (None, Seq())
    }
  }

  val generateFieldsExtendsFromAndMixinsCached = Memo.mutableHashMapMemo[DiaEntity, (Option[DiaClassRefBase], Seq[DiaClassRefBase])] {generateFieldsExtendsFromAndMixins}

  def generateExtendsFromField(c: DiaEntity): Option[DiaClassRefBase] = generateFieldsExtendsFromAndMixinsCached(c)._1

  def generateMixinsField(c: DiaEntity): Seq[DiaClassRefBase] = generateFieldsExtendsFromAndMixinsCached(c)._2

  val ScalaKeywords = "abstract,case,catch,class,def,do,else,extends,false,final,finally,for,forSome,if,implicit,import,lazy,match,new,null,object,override,package,private,protected,return,sealed,super,this,throw,trait,try,true,type,val,var,while,with,yield".split(",").toSet

  def sanitizeName(n: String): String = if (ScalaKeywords.contains(n)) s"`$n`" else n

  def emitClass(c: DiaEntity, f: DiaFile): EmittedParts = {
    val indent = "  "
    def genClass = (c.classType match {
      case DiaEntityType.Class => "class"
      case DiaEntityType.Object | DiaEntityType.Enumeration => "object"
      case DiaEntityType.Trait => "trait"
    }) + s" ${sanitizeName(c.ref.name)}"

    def genExtends = (if (c.classType == DiaEntityType.Enumeration) DiaClassRefBase.fromStringUnchecked("Enumeration") else generateExtendsFromField(c)).
      map(ef => s" extends ${ef.emitCode()}").getOrElse("")

    def genMixins = {
      val mixins = generateMixinsField(c)
      if (mixins.isEmpty) "" else mixins.map(_.emitCode()).mkString(" with ", " with ", "")
    }

    def genVisibility(v: DiaVisibility) = if (v == DiaVisibility.Public) "" else v.code + " "

    def genTypePart(t: DiaClassRefBase): String = ": " + t.emitCode()

    def genOptTypePart(t: Option[DiaClassRefBase]): String = t.map(genTypePart).getOrElse("")

    def genAttributes: Seq[String] = c.attributes.map { a: DiaAttribute =>
      indent +
        genVisibility(a.visibility) +
        (if (a.isLazy) "lazy " else "") +
        (if (a.isVal) "val " else "var ") +
        sanitizeName(a.name) +
        genOptTypePart(a.aType) +
        " = " + (if (a.isVal) "???" else "_") +
        ""
    }

    def genParameter(p: DiaOperationParameter): String = sanitizeName(p.name) + genOptTypePart(p.pType)

    def genOperations: Seq[String] = c.operations.map { o: DiaOperationDescriptor =>
      val name = sanitizeName(o.name)
      indent +
        genVisibility(o.visibility) +
        "def " +
        name + (if (name.last.isLetterOrDigit) "" else " ") +
        "(" +
        o.parameters.map(genParameter).mkString(", ") +
        ")" +
        o.oType.map(t => genTypePart(t) + " = ???").getOrElse("{ ??? }") +
        ""
    }

    def genImportsForClassRef(cr: DiaClassRefBase): Seq[String] = {
      val isEnumeration = cr match {
        case x: DiaUserClassRef => f.isEnumeration(x.fullName)
        case _ => false
      }

      if (cr.emitCodeWithFullName() == c.ref.emitCodeWithFullName() ||
        (cr.inUserPackage(c.ref.inPackage) && !isEnumeration)) {
        Seq()
      } else {
        cr match {
          case x: DiaUserClassRef =>
            Seq(if (isEnumeration) s"${x.fullName}._" else x.fullName)
          case x: DiaGenericClassRef => genImportsForClassRef(x.base) ++ x.params.flatMap(genImportsForClassRef)
          case x: DiaTupleClassRef => x.params.flatMap(genImportsForClassRef)
          case _ => Seq()
        }
      }
    }

    def genImportsForClassRefOpt(cr: Option[DiaClassRefBase]): Seq[String] = cr.map(genImportsForClassRef).getOrElse(Seq())

    def genImportsForMixins: Seq[String] =
      (if (generateExtendsFromField(c).isEmpty) Seq() else Seq(generateExtendsFromField(c).get.emitCodeWithFullName())) ++
        generateMixinsField(c).map(_.emitCodeWithFullName())

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
      cb |> { cb: CodeBuilder => if (c.classType == DiaEntityType.Enumeration) {
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

  def emitParts(f: DiaFile): Seq[EmittedParts] = f.entities.map(emitClass(_, f))
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
