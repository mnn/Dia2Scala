package tk.monnef.dia2scala

import org.scalatest.FlatSpec

class TestDiaFile extends FlatSpec {
  final val testTypes = Seq(
    "UserClass",
    "somePack.UserClass",
    "somePack.withAPath.UserClass",
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

    val exp = Seq(expUser, expUser, expUser, expScala, expScala, expGeneric, expGeneric, expFunction, expFunction, expTuple, expFunction, expGeneric, expFunction, expFunction, expFunction, expTuple, expTuple, expTuple, expTuple)
    assert(testTypes.size == exp.size)

    testTypes.zip(exp).foreach { case (sType, expected) =>
      println(s"DiaFile testing: $sType")
      assert(t(sType) == expected, sType)
    }
  }

  "DiaClassRefBase" should "be able to parse any type" in {
    val bigMap = DiaGenericClassRef(DiaScalaClassRef("Map"), Seq(
      DiaFunctionClassRef(Seq(DiaScalaClassRef("Int"), DiaScalaClassRef("Byte")), DiaTupleClassRef(Seq(DiaScalaClassRef("String"), DiaScalaClassRef("Int")))),
      DiaFunctionClassRef(Seq(DiaScalaClassRef("Byte")), DiaScalaClassRef("String"))
    ))

    val exp = Seq[DiaClassRefBase](
      DiaUserClassRef("UserClass", ""),
      DiaUserClassRef("UserClass", "somePack"),
      DiaUserClassRef("UserClass", "somePack.withAPath"),
      DiaScalaClassRef("String"),
      DiaScalaClassRef("Unit"),
      DiaGenericClassRef(DiaScalaClassRef("Seq"), Seq(DiaScalaClassRef("Int"))),
      DiaGenericClassRef(DiaScalaClassRef("Map"), Seq(
        DiaGenericClassRef(DiaScalaClassRef("Seq"), Seq(DiaScalaClassRef("String"))),
        DiaGenericClassRef(DiaScalaClassRef("Map"), Seq(
          DiaGenericClassRef(DiaScalaClassRef("Set"), Seq(
            DiaGenericClassRef(DiaScalaClassRef("Map"), Seq(
              DiaScalaClassRef("Int"),
              DiaGenericClassRef(DiaScalaClassRef("Map"), Seq(
                DiaScalaClassRef("Char"),
                DiaScalaClassRef("Byte")
              ))
            ))
          )),
          DiaScalaClassRef("String")
        ))
      )),
      DiaFunctionClassRef(Seq(DiaScalaClassRef("Int"), DiaScalaClassRef("String")), DiaScalaClassRef("Unit")),
      DiaFunctionClassRef(Seq(
        DiaTupleClassRef(Seq(DiaScalaClassRef("Int"), DiaScalaClassRef("String"))),
        DiaFunctionClassRef(Seq(DiaScalaClassRef("Double")), DiaScalaClassRef("Int"))
      ), DiaScalaClassRef("Float")),
      DiaTupleClassRef(Seq(
        DiaFunctionClassRef(Seq(), DiaScalaClassRef("Unit")),
        DiaFunctionClassRef(Seq(DiaScalaClassRef("Int")), DiaScalaClassRef("String"))
      )),
      DiaFunctionClassRef(Seq(DiaScalaClassRef("Int")), DiaScalaClassRef("Char")),
      bigMap,
      DiaFunctionClassRef(Seq(bigMap), DiaGenericClassRef(DiaScalaClassRef("Seq"), Seq(DiaFunctionClassRef(Seq(), DiaScalaClassRef("Short"))))),
      DiaFunctionClassRef(Seq(), DiaScalaClassRef("Unit")),
      DiaFunctionClassRef(Seq(DiaEmptyClassRef), DiaScalaClassRef("String")),
      DiaTupleClassRef(Seq(DiaScalaClassRef("Int"))),
      DiaTupleClassRef(Seq(DiaFunctionClassRef(Seq(DiaScalaClassRef("Int")), DiaScalaClassRef("Char")))),
      DiaTupleClassRef(Seq(DiaScalaClassRef("Int"), DiaScalaClassRef("Char"), DiaScalaClassRef("Byte"))),
      DiaTupleClassRef(Seq(
        DiaTupleClassRef(Seq(
          DiaScalaClassRef("Int"),
          DiaTupleClassRef(Seq(
            DiaScalaClassRef("Char"),
            DiaScalaClassRef("Byte")
          ))
        )),
        DiaTupleClassRef(Seq(DiaScalaClassRef("Short"))),
        DiaTupleClassRef(Seq(
          DiaScalaClassRef("Long"),
          DiaTupleClassRef(Seq(DiaScalaClassRef("Int")))
        ))
      ))
    )

    testTypes.zip(exp).foreach { case (testType, expected) =>
      println(s"DiaClassRefBase testing: $testType")
      assert(DiaClassRefBase.fromStringUnchecked(testType).get == expected, testType)
      println("... fine")
    }
  }

  "DiaGenericClassRef" should "create representation from string" in {
    def t(i: String) = DiaGenericClassRef.fromString(i)
    assert(t("Seq[Int]") == DiaGenericClassRef(DiaScalaClassRef("Seq"), Seq(DiaScalaClassRef("Int"))))
    assert(t("CustomMap[Jaffa, Box]") == DiaGenericClassRef(DiaUserClassRef("CustomMap", ""), Seq(DiaUserClassRef("Jaffa", ""), DiaUserClassRef("Box", ""))))
  }
}
