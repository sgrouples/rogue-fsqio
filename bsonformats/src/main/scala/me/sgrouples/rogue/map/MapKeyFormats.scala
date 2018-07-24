package me.sgrouples.rogue.map

import me.sgrouples.rogue.map
import org.bson.types.ObjectId

trait MapKeyFormats {

  implicit object StringMapKeyFormat extends DefaultMapKeyFormat(identity)

  implicit object LongMapKeyFormat extends DefaultMapKeyFormat(_.toLong)

  implicit object IntMapKeyFormat extends DefaultMapKeyFormat(_.toInt)

  implicit object ObjectIdMapKeyFormat extends DefaultMapKeyFormat(new ObjectId(_))

  implicit def objectIdSubtypeMapKeyFormat[S <: ObjectId]: MapKeyFormat[S] =
    map.MapKeyFormat[S](ObjectIdMapKeyFormat.read(_).asInstanceOf[S])

}

object MapKeyFormats extends MapKeyFormats
