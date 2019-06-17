
package io.fsq.rogue.index

import org.bson.{ BsonInt32, BsonString, BsonValue }

case class IndexModifier(value: BsonValue) {
  def bsonValue: BsonValue = value
}

object Asc extends IndexModifier(new BsonInt32(1))
object Desc extends IndexModifier(new BsonInt32(-1))
object TwoD extends IndexModifier(new BsonString("2d"))
object Hashed extends IndexModifier(new BsonString("hashed"))
object Text extends IndexModifier(new BsonString("text"))

