package tk.monnef.dia2scala

import scala.util.control.NonFatal
import scalaz.{\/-, -\/, \/}
import scalaz.syntax.either._

object Utils {
  def wrapErrorToJunction[A](c: => A): \/[String, A] =
    try {c.right}
    catch {case NonFatal(e) => e.getMessage.left}

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
          case l@ -\/(_) => l
          case \/-(accSeq: Seq[T]) => processItem(accSeq, item)
        }
    }
  }
}
