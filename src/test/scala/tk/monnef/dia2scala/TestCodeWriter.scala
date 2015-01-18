package tk.monnef.dia2scala

import org.scalatest.FlatSpec

class TestCodeWriter extends FlatSpec {
  "breakGroupsContainingDifferentPackages" should "break groups containing classes from different packages" in {
    val (a, b, c, d) = (
      EmittedParts("A", "1", "", "A.scala", Seq()),
      EmittedParts("B", "2", "", "B.scala", Seq()),
      EmittedParts("C", "1", "", "C.scala", Seq()),
      EmittedParts("D", "2", "", "D.scala", Seq())
      )

    def t(input: Seq[Seq[EmittedParts]], expected: Seq[Seq[EmittedParts]]) {
      val result = CodeWriter.breakGroupsContainingDifferentPackages(input)
      assert(result == expected, result.map(_.map(_.fullName)))
    }

    t(Seq(Seq(a, b, c)), Seq(Seq(a), Seq(b), Seq(c)))
    t(Seq(Seq(a, c)), Seq(Seq(a, c)))
    t(Seq(Seq(a, b, d)), Seq(Seq(a), Seq(b, d)))
  }
}
