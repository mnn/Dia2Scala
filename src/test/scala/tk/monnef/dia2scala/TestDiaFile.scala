package tk.monnef.dia2scala

import org.scalatest.FlatSpec

class TestDiaFile extends FlatSpec {
  "DiaGenericClassRef" should "create representation from string" in {
    def t(i: String) = DiaGenericClassRef.fromString(i)
    assert(t("Seq[Int]") == DiaGenericClassRef(DiaScalaClassRef("Seq"), Seq(DiaScalaClassRef("Int"))))
    assert(t("CustomMap[Jaffa, Box]") == DiaGenericClassRef(DiaUserClassRef("CustomMap", ""), Seq(DiaUserClassRef("Jaffa", ""), DiaUserClassRef("Box", ""))))
  }
}
