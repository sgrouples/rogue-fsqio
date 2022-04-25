package me.sgrouples.rogue.map

import me.sgrouples.rogue.map
import org.bson.types.ObjectId

trait MapKeyFormats:
  given MapKeyFormat[String] = DefaultMapKeyFormat[String](identity)
  given MapKeyFormat[Long] = DefaultMapKeyFormat[Long](_.toLong)
  given MapKeyFormat[Int] = DefaultMapKeyFormat[Int](_.toInt)
  given ObjectIdMapKeyFormat: MapKeyFormat[ObjectId] = DefaultMapKeyFormat[ObjectId](new ObjectId(_))
  implicit def objectIdSubtypeMapKeyFormat[S <: ObjectId]: MapKeyFormat[S] =
    map.MapKeyFormat[S](ObjectIdMapKeyFormat.read(_).asInstanceOf[S])

object MapKeyFormats extends MapKeyFormats