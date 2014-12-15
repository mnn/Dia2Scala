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
    val intend = "  "
    def genClass = s"class ${c.name}"
    def genExtends = !c.extendsFrom.isEmpty ? s" extends ${c.extendsFrom}" | ""
    def genMixins = if(c.mixins.isEmpty) "" else  c.mixins.mkString(" with ", " with ", "")
    def genVisibility(v: DiaVisibility) = if (v == DiaVisibility.Public) "" else v.code + " "
    def genType(t: Option[String]): String = t.map(": " + _) | ""
    def genAttributes: Seq[String] = c.attributes.map { a: DiaAttribute =>
      intend +
        genVisibility(a.visibility) +
        "var " +
        a.name +
        genType(a.aType) +
        " = _" +
        ""
    }
    def genParameter(p: DiaOperationParameter): String = p.name + genType(p.pType)
    def genOperations: Seq[String] = c.operations.map { o: DiaOperationDescriptor =>
      intend +
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

    val code = genClass +
      genExtends +
      genMixins +
      " {" +
      nl +
      genAttributes.mkString(nl) +
      dnl +
      genOperations.mkString(dnl) +
      nl +
      "}" +
      nl

    EmittedClass(
      c.name,
      c.inPackage,
      code,
      c.name.capitalizeFirst + ".scala"
    )
  }

  def emitClasses(cs: Seq[DiaClass]): Seq[EmittedClass] = cs map emitClass
}