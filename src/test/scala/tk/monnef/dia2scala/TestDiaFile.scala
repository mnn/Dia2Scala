package tk.monnef.dia2scala

import org.scalatest.FlatSpec

class TestDiaFile extends FlatSpec {
  final val testTypes = Seq(
    "UserClass",
    "String",
    "Unit",
    "Seq[Int]",
    "Map[  Seq[String],Map[Set[Map[Int,Map[Char,Byte]]],String]  ]",
    "(Int, String) => Unit",
    "((Int, String), Double => Int) => Float",
    "(()=>Unit,Int=>String)",
    "Int => Char",
    "Map[(Int, Byte) => (String,Int), Byte => String]",
    "Map[(Int, Byte) => (String,Int), Byte => String] => Seq[() => Short]",
    "() => Unit",
    "=> String",
    "(Int)",
    "(Int => Char)",
    "(Int, Char, Byte)",
    "((Int,(Char,Byte)),(Short),(Long,(Int)))"
  )

  "DiaFile" should "properly detect outer most type" in {
    import DiaClassRefBase._
    // scala, generic, function, tuple
    def t(i: String): (Boolean, Boolean, Boolean, Boolean) =
      (
        isScalaClass(i),
        isGenericClass(i),
        isFunctionClass(i),
        isTupleClass(i)
        )

    val expUser = (false, false, false, false)
    val expScala = (true, false, false, false)
    val expGeneric = (false, true, false, false)
    val expFunction = (false, false, true, false)
    val expTuple = (false, false, false, true)

    val exp = Seq(expUser, expScala, expScala, expGeneric, expGeneric, expFunction, expFunction, expTuple, expFunction, expGeneric, expFunction, expFunction, expFunction, expTuple, expTuple, expTuple, expTuple)
    assert(testTypes.size == exp.size)

    testTypes.zip(exp).foreach { case (sType, expected) =>
      assert(t(sType) == expected, sType)
    }
  }

  "DiaGenericClassRef" should "create representation from string" in {
    def t(i: String) = DiaGenericClassRef.fromString(i)
    assert(t("Seq[Int]") == DiaGenericClassRef(DiaScalaClassRef("Seq"), Seq(DiaScalaClassRef("Int"))))
    assert(t("CustomMap[Jaffa, Box]") == DiaGenericClassRef(DiaUserClassRef("CustomMap", ""), Seq(DiaUserClassRef("Jaffa", ""), DiaUserClassRef("Box", ""))))
  }
}
