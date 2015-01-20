Diagram example
===============
This page shows input diagram and output code.

Diagram
-------

![](https://github.com/mnn/Dia2Scala/blob/master/other/example01.png)


Output
------

tk/monnef/game/entity/Alive.scala
```scala
package tk.monnef.game.entity

trait Alive {
  var hp: Int = _
  var dead: Boolean = _

  def hurt(amount: Int){ ??? }

  def kill(){ ??? }

  def maxHp(): Int = ???

}
```

tk/monnef/game/entity/Entity.scala
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

tk/monnef/game/entity/Inventory.scala
```scala
package tk.monnef.game.entity

import tk.monnef.game.entity.Item._

trait Inventory {
  var inventory: Seq[Option[Item]] = _

}
```

tk/monnef/game/entity/Item.scala
```scala
package tk.monnef.game.entity

object Item extends Enumeration {
  type Item = Value
  val LetterOpener, PurpleDonut = Value

  def damage(of: Item): Int = ???

}
```

tk/monnef/game/entity/MeleeDamageDealer.scala
```scala
package tk.monnef.game.entity

trait MeleeDamageDealer {

  def hitDamage(): Int = ???

}
```

tk/monnef/game/Game.scala
```scala
package tk.monnef.game

import tk.monnef.game.Game._
import tk.monnef.game.entity.Hero
import tk.monnef.game.entity.Skeleton

class Game {
  val hero: Hero = ???
  var monsters: Seq[Skeleton] = _

}


object Game {

  def load(path: String): Game = ???

}
```

