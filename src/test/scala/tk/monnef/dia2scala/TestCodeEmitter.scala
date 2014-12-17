package tk.monnef.dia2scala

import org.scalatest.FlatSpec
import scalaz._
import Scalaz._

class TestCodeEmitter extends FlatSpec {
  "CodeBuilder" should "build code respecting maximum of successive blank lines with blank input" in {
    val b = new CodeBuilder
    b.maximumOfSuccessiveBlankLines = 0
    b.addLine()
    b.addLine()
    assert(b.convertToString == "")
  }

  "CodeBuilder" should "build code respecting maximum of successive blank lines with blank input and non-zero maximum" in {
    val newLines = 50
    def test(max: Int) {
      assert(max < newLines)
      val b = new CodeBuilder
      b.maximumOfSuccessiveBlankLines = max
      for (i <- 0 to newLines) b.addLine()
      assert(b.convertToString == "\n" * max, max)
    }
    test(0)
    test(1)
    test(2)
    test(10)
    test(49)
  }

  it should "build code respecting maximum of successive blank lines with non-blank input" in {
    val newLines = 50
    def test(max: Int, head: Int) {
      assert(max < newLines)
      val b = new CodeBuilder
      b.maximumOfSuccessiveBlankLines = max
      for (i <- 0 to newLines) {
        if (i == head) b.addLine("line")
        b.addLine()
      }
      val linesAfter = Math.min(newLines - head, max)
      assert(b.convertToString == "\n" * head + "line\n" + "\n" * linesAfter, Seq(max, head, linesAfter, b.computeEndingBlankLines))
    }

    for {
      head <- Seq(0, 1, 2, 3, 5)
      max <- Seq(0, 1, 2, 3, 4, 5, 10)
      if max > head
    } test(max, head)
  }
}
