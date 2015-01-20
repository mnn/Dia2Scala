Notation
========
Inspired by [SO answer](https://stackoverflow.com/questions/7815983/any-uml-tools-for-scala/7817723#7817723) and its source - thesis [Evaluierung des Einsatzes von Scala bei der Entwicklung für die Android-Plattform](http://tribosmap.googlecode.com/files/MRachimowDiplA.pdf) by Meiko Rachimow.


Inheritance
-----------

![](https://github.com/mnn/Dia2Scala/blob/master/other/scalaUML1.png)

```scala
trait TraitB; trait TraitC; trait TraitDBase

abstract class ClassABase
class ClassA extends ClassABase with TraitB with TraitC
trait TraidD extends TraitDBase
trait TraitI { self: TraitC => }
```


Mutability
----------

![](https://github.com/mnn/Dia2Scala/blob/master/other/scalaUML1b.png)
```scala
class ClassK(val count: Int, cache: Int)

class ClassJ(var field: Option[Int])

class ClassL { lazy val sum = (1 to 1000000).sum }
```



Object
------
![](https://github.com/mnn/Dia2Scala/blob/master/other/scalaUML2.png)
```scala
class ClassF {
	import ClassF._
	
	def instanceMethod: Double = 0
}

object ClassF {
	def staticMethod: ClassF = null
}
```


Types
-----
Please remember that most of following language properties are **not** supported by Dia2Scala. This sub-page is about notation, not currently supported features (it might be implemented in future though).

![](https://github.com/mnn/Dia2Scala/blob/master/other/scalaUML2b.png)

```scala
abstract class ClassG[T <: Any] {
  def process: T
}

object ClassG {
  def method[S](x: Seq[S]): S = x.head
}

class ClassGChild extends ClassG[Int] {
  def process = 2
}

class ClassH[T <: { def op: String }](val data: T) {
  def applyOp: String = data.op
}

println(ClassG.method(Seq("a", "b")))
println(new ClassGChild().process)
println(new ClassH(new { def op = "Whoo" }).applyOp)
```

