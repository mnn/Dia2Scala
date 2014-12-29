import scala.annotation.tailrec

val input = "Seq[String],Map[Set[Map[Int,Map[Char,Byte]]],String]"

def chop(i: String): Seq[String] = {
  var res = Seq[String]()
  var buff = ""
  var rest = i
  var counter = 0
  while (rest.nonEmpty) {
    rest(0) match {
      case '[' => counter += 1; buff += '['
      case ']' => counter -= 1; buff += ']'
      case ',' =>
        if (counter == 0) {
          res :+= buff
          buff = ""
        } else buff += ','
      case ' ' =>
      case c: Char => buff += c
    }
    rest = rest.tail
  }
  res ++ Seq(buff)
}

def chopRec(i: String): Seq[String] = {
  @tailrec def loop(rest: String, buff: String, counter: Int, res: Seq[String]): Seq[String] =
    if (rest.isEmpty) res ++ Seq(buff)
    else {
      val (newCounter, appendChar, flipBuff) = rest.head match {
        case '[' => (counter + 1, true, false)
        case ']' => (counter - 1, true, false)
        case ',' => if (counter == 0) (0, false, true) else (counter, true, false)
        case ' ' => (counter, false, false)
        case _ => (counter, true, false)
      }
      loop(
        rest.tail,
        if (appendChar) buff + rest.head
        else if (flipBuff) "" else buff,
        newCounter,
        if (flipBuff) res ++ Seq(buff) else res
      )
    }

  loop(i, "", 0, Seq())
}

val r1 = chop(input).mkString("  ;  ")
val r2 = chop(input).map(a => a.dropWhile(_ != '[').drop(1).dropRight(1)).map(chop).map(_.mkString("  ;  ")).mkString("  //  ")

val r1b = chopRec(input).mkString("  ;  ")
val r2b = chopRec(input).map(a => a.dropWhile(_ != '[').drop(1).dropRight(1)).map(chopRec).map(_.mkString("  ;  ")).mkString("  //  ")
assert(r1 == r1b)
assert(r2 == r2b)
