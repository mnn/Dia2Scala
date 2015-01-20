Diagram example
===============
This page shows input diagram and output code.

![](https://github.com/mnn/Dia2Scala/blob/master/other/example01.png)

**TODO**: write script to export code

```scala
package tk.monnef.game.entity

class Entity {
  var x: Float = _
  var y: Float = _

}


class EntityLiving extends Entity with Alive with MeleeDamageDealer {

}


class Hero extends EntityLiving with Inventory {

  override def kill(){ ??? }

  override def hitDamage(): Int = ???

  override def maxHp(): Int = ???

}


class Skeleton extends EntityLiving {
  override val hitDamage: Int = ???
  override val maxHp: Int = ???

}
```
