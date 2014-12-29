import scala.annotation.tailrec

val in = "(Int, String) => Unit"
val in2 = "((Int, String), Double => Int) => Float"
val in3 = "(()=>Unit,Int=>String)"
val in4 = "Int => Char"
val in5 = "Map[Int => String, Char]"
val in6 = "Seq[Char] => String"
val in7 = "Map[(Int, Byte) => (String,Int), Byte => String]"
val in8 = "Map[(Int, Byte) => (String,Int), Byte => String] => Seq[() => Short]"
val in9 = "() => Unit"
val in10 = "=> String"

def chopAllEnclosedBraces(i: String): String = {
  @tailrec def loop(in: String, count: Int, res: String): String =
    if (in.isEmpty) {
      if (count != 0) throw new RuntimeException(s"Type description has even count of parenthesis, critical error - '$i'.")
      res
    } else {
      in.head match {
        case '(' => loop(in.tail, count + 1, res)
        case ')' => loop(in.tail, count - 1, res)
        case c: Char =>
          val newRes = if (count == 0) res + c else res
          loop(in.tail, count, newRes)
      }
    }

  loop(i, 0, "")
}

def chopFunction(i: String): Seq[String] = {
  @tailrec def loop(in: String, count: Int, buff: String, lastWasEq: Boolean, res: Seq[String]): Seq[String] =
    if (in.isEmpty) {
      if (count != 0) throw new RuntimeException(s"Type description has even count of parenthesis, critical error - '$i'.")
      res :+ buff
    } else {
      val lowestLevel = count == 0
      val (counterDiff, append, flipBuff, eq) = in.head match {
        case '(' | '[' => (1, true, false, false)
        case ')' | ']' => (-1, true, false, false)
        case '=' => (0, !lowestLevel, false, lowestLevel)
        case '>' => (0, !lowestLevel, lastWasEq, false)
        case ' ' => (0, false, false, false)
        case c: Char => (0, true, false, false)
      }
      val newBuff = buff + (if (append) in.head else "")
      loop(
        in.tail,
        count + counterDiff,
        if (!flipBuff) newBuff else "",
        eq,
        if (flipBuff) res :+ newBuff else res
      )
    }

  loop(i, 0, "", lastWasEq = false, Seq())
}

def isFunction(i: String): Boolean = chopFunction(i).size == 2

val inputSeq = Seq(in, in2, in3, in4, in5, in6, in7, in8, in9, in10)
val r1 = inputSeq.map(chopAllEnclosedBraces)
val r2 = inputSeq.map(chopFunction)
def formatSeq(l: String, x: Seq[String]) { println(s"$l:\n" + x.mkString("\n")) }
def formatSeqSeq(l: String, x: Seq[Seq[String]]) { println(s"$l: " + x.map(_.mkString("  ;  ")).mkString("\n")) }
formatSeq("inputSeq", inputSeq)
formatSeq("r1", r1)
formatSeqSeq("r2", r2)
formatSeq("isFunction", inputSeq.map(x => isFunction(x).toString + ": " + x))
