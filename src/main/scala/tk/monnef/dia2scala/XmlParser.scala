package tk.monnef.dia2scala

import java.util.zip.GZIPInputStream

import scala.util.Try

import java.io.{InputStreamReader, FileInputStream, InputStream, File}

object XmlParser {

  import XmlParserHelper._
  import scalaz._
  import Scalaz._

  def parseFile(file: File, isPacked: Boolean): \/[String, DiaFile] = {
    getXmlStream(file, isPacked) match {
      case scala.util.Failure(t) => s"Unable to create stream reader: ${t.getMessage}".left
      case scala.util.Success(xmlStream) =>
        val xml = scala.xml.XML.load(xmlStream)
        /*
        * Process list:
        * - xml
        * - parse and process packages
        * - parse and semi-process classes: name, compute package, attributes (not yet from association connections) and operators
        * - parse and process generalization
        * - parse and process <<implements>>
        * - parse and process <<mixin>>
        * - parse and process associations
        */
        DiaFile(Seq(), Seq()).right
    }
  }
}

object XmlParserHelper {

  import scala.xml.{Node, NodeSeq, Elem}
  import scalaz._
  import scalaz.syntax.std.tuple._
  import Scalaz.ToIdOps

  def getXmlStream(file: File, isPacked: Boolean): Try[InputStreamReader] =
    Try {
      new FileInputStream(file).
        |> { a => if (isPacked) new GZIPInputStream(a) else a: InputStream}.
        |> {new InputStreamReader(_)}
    }

  def extractObjectByType(e: Elem, objType: String): NodeSeq = e \\ "dia:object" filter {_ \@ "type" == objType}

  def extractDiaAttributeByName(n: Node, name: String): Node = (n \\ "dia:attribute" filter {_ \@ "name" == name}).head

  def extractDiaAttributePoint(n: Node, name: String): (Double, Double) =
    (extractDiaAttributeByName(n, name) \ "dia:point" \@ "val").
      split(',').toList |> { case a :: b :: _ => Seq(a, b).map(_.toDouble)} |> { case a :: b :: _ => (a, b)}

  def processPackages(e: Elem): \/[String, Seq[DiaPackage]] = {
    extractObjectByType(e, "UML - LargePackage").map { case node =>
      val (w, h) = extractDiaAttributePoint(node, "elem_corner")
      // TODO
    }
  }
}
