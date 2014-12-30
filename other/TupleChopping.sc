import scala.annotation.tailrec

val t = Seq(
  "(Int)",
  "(Int => Char)",
  "(Int, Char, Byte)",
  "((Int,(Char,Byte)),(Short),(Long,(Int)))"
)

def chopRec(i: String): Seq[String] = {
  @tailrec def loop(rest: String, buff: String, counter: Int, res: Seq[String]): Seq[String] =
    if (rest.isEmpty) res ++ Seq(buff)
    else {
      val (newCounter, appendChar, flipBuff) = rest.head match {
        case '(' => (counter + 1, true, false)
        case ')' => (counter - 1, true, false)
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


t.foreach { case in =>
  val r = chopRec(in.drop(1).dropRight(1))
  println(s"$in\n${r.mkString("  ;  ")}\n")
}


