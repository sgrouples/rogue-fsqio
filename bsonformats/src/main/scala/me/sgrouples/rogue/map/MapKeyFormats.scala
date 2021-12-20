package me.sgrouples.rogue.map

import me.sgrouples.rogue.map
import org.bson.types.ObjectId

trait MapKeyFormats {

  object _stringMapKeyFormat extends DefaultMapKeyFormat(identity)
  implicit def StringMapFormat:MapKeyFormat[String] = _stringMapKeyFormat

  object _longMapKeyFormat extends DefaultMapKeyFormat(_.toLong)
  implicit def LongMapKeyFormat:MapKeyFormat[Long] = _longMapKeyFormat

  object _intMapKeyFormat extends DefaultMapKeyFormat(_.toInt)
  implicit def IntMapKeyFormat:MapKeyFormat[Int] = _intMapKeyFormat

  object _objectIdMapKeyFormat
      extends DefaultMapKeyFormat(new ObjectId(_))

  implicit def ObjectIdMapKeyFormat:MapKeyFormat[ObjectId] = _objectIdMapKeyFormat

  implicit def objectIdSubtypeMapKeyFormat[S <: ObjectId]: MapKeyFormat[S] =
    map.MapKeyFormat[S](ObjectIdMapKeyFormat.read(_).asInstanceOf[S])

}

object MapKeyFormats extends MapKeyFormats
