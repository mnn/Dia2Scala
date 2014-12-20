package tk.monnef.dia2scala

import org.scalatest.FlatSpec

class TestUtils extends FlatSpec {
  "topologicalSortWithGrouping" should "do topo sort" in {
    val edges = Seq("A" -> "B", "G" -> "A", "H" -> "A", "C" -> "D", "D" -> "E", "I" -> "F")
    val res = Utils.topologicalSortWithGrouping(edges)
    val setA = Set(Seq("B", "A", "G", "H"), Seq("E", "D", "C"), Seq("F", "I"))
    val setB = Set(Seq("B", "A", "H", "G"), Seq("E", "D", "C"), Seq("F", "I"))
    assert(res.toSet == setA || res.toSet == setB)
  }

  it should "not crash on empty input" in {
    val res = Utils.topologicalSortWithGrouping(Seq())
    assert(res.isEmpty)
  }

  it should "crash on a perfect cycle" in {
    intercept[RuntimeException] {
      Utils.topologicalSortWithGrouping(Seq("A" -> "B", "B" -> "A"))
    }
    intercept[RuntimeException] {
      Utils.topologicalSortWithGrouping(Seq("A" -> "B", "B" -> "C", "C" -> "A"))
    }
  }

  it should "crash on an imperfect cycle" in {
    intercept[RuntimeException] {
      Utils.topologicalSortWithGrouping(Seq("A" -> "B", "B" -> "A", "B" -> "Root"))
    }
    intercept[RuntimeException] {
      Utils.topologicalSortWithGrouping(Seq("A" -> "B", "B" -> "C", "C" -> "A", "A" -> "Root"))
    }
    intercept[RuntimeException] {
      Utils.topologicalSortWithGrouping(Seq("A" -> "B", "B" -> "C", "C" -> "A", "A" -> "Root", "Ground" -> "C"))
    }
    intercept[RuntimeException] {
      Utils.topologicalSortWithGrouping(Seq("A" -> "B", "B" -> "C", "C" -> "A", "A" -> "Root", "Ground" -> "A"))
    }
  }
}
