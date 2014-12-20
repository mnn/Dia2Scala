package tk.monnef.dia2scala

import scala.util.control.NonFatal
import scalaz.{\/-, -\/, \/}
import scalaz.syntax.either._
import scalaz.syntax.optional._
import scalaz.syntax.std.all._
import scalaz.Scalaz.ToIdOps

//import scalaz.syntax.all._

object Utils {
  def wrapErrorToJunction[A](c: => A): \/[String, A] =
    try {
      c.right
    }
    catch {
      case NonFatal(e) =>
        val sTrace = e.getStackTrace.mkString("", "\n", "\n")
        s"${e.getMessage}\n$sTrace".left
    }

  def seqOfTwoToTuple[T](l: Seq[T]): (T, T) = (l(0), l(1))

  def liftFirstError[T](x: Seq[\/[String, T]]): \/[String, Seq[T]] = {
    def processItem(accSeq: Seq[T], item: \/[String, T]): \/[String, Seq[T]] = {
      item match {
        case -\/(err: String) => err.left[Seq[T]]
        case \/-(t) => (accSeq :+ t).right[String]
      }
    }
    x.foldLeft(Seq[T]().right[String]) {
      case (acc: \/[String, Seq[T]], item: \/[String, T]) =>
        acc match {
          case -\/(_) => acc
          case \/-(accSeq: Seq[T]) => processItem(accSeq, item)
        }
    }
  }

  def wrapNonEmptyStringToSome(s: String): Option[String] = (s != null && s.nonEmpty) ? s.some | None

  implicit class StringPimps(s: String) {
    def capitalizeFirst: String =
      if (s.size < 1) s
      else s.take(1).toUpperCase + s.drop(1)

    def toOptSeq: Seq[String] = if (s.isEmpty) Seq() else Seq(s)
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {op(p)} finally {p.close()}
  }

  implicit class SeqPimps[T](s: Seq[T]) {
    def dropWhileRight(p: T => Boolean): Seq[T] = s.reverse.dropWhile(p).reverse

    def -(o: T) = s.filter(_ != o)
  }

  def getNodesWithMultipleOutputs[T](inputEdges: Seq[(T, T)]): Seq[(T, Seq[(T, T)])] = {
    val sourceAndEdgeStartingFromIt: Seq[(T, Seq[(T, T)])] = for {
      e <- inputEdges
      from = e._1
    } yield (from, inputEdges.filter { case (f, t) => f == from})
    def removeNode(i: (T, Seq[(T, T)])): Seq[(T, T)] = i._2
    val groupedSourceAndEdgeStartingFromIt: Seq[(T, Seq[(T, T)])] = sourceAndEdgeStartingFromIt.groupBy(_._1).
      |> { grouped: Map[T, Seq[(T, Seq[(T, T)])]] => grouped.map { case (node, nodeWithEdges) => (node, nodeWithEdges.flatMap(removeNode))}.toSeq}
    groupedSourceAndEdgeStartingFromIt.filter { case (node, edges) => edges.size > 1}
  }

  def topologicalSortWithGrouping(edges: Seq[(String, String)]): Seq[Seq[String]] =
    if (edges.isEmpty) Seq()
    else {
      val nodes = edges.flatMap(e => Seq(e._1, e._2)).distinct
      var sorted = nodes.filter(n => !edges.exists { case (from, to) => from == n})
      var nonSorted = nodes diff sorted
      var res: Seq[Seq[String]] = sorted.map(n => Seq(n))
      var processedEdges = Seq[(String, String)]()

      if (sorted.isEmpty) throw new RuntimeException(s"Nothing to sort - no starting node found, probably a cycle. $edges")

      val multiSourceNodes = getNodesWithMultipleOutputs(edges)
      if (multiSourceNodes.nonEmpty) {
        throw new RuntimeException(s"Found multiple outputs from a single node, cannot proceed. $multiSourceNodes")
      }

      while (nonSorted.nonEmpty) {
        for {
          currNode <- sorted
          children = edges.filter { case (from, to) => to == currNode}.map(_._1)
          childrenNotSorted = children.filter(!sorted.contains(_))
        } {
          for {child <- childrenNotSorted} {
            val curRoute = child -> currNode
            if (processedEdges.contains(curRoute)) throw new RuntimeException(s"Detected cycle, halting ($curRoute).")
            sorted :+= child
            nonSorted -= child
            res = res.map { case resChain =>
              if (resChain.contains(currNode)) {
                resChain :+ child
              } else resChain
            }
            processedEdges :+= curRoute
          }
        }
      }
      if (processedEdges.toSet != edges.toSet) {
        throw new RuntimeException(s"Not all edges traversed, looks like a cycle. Not used: ${edges diff processedEdges}")
      }

      res
    }
}
