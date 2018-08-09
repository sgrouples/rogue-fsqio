package me.sgrouples.rogue.cc
import me.sgrouples.rogue.map.{ MapKeyFormat, MapKeyFormats }
import org.bson.types.ObjectId
import scala.language.experimental.{ macros => ms }

package object macros {
  implicit def gen[T]: MacroBsonFormat[T] = macro MacroCCGenerator.genImpl[T]

  implicit val StringMapKeyFormat = MapKeyFormats.StringMapKeyFormat
  implicit val LongMapKeyFormat = MapKeyFormats.LongMapKeyFormat
  implicit val IntMapKeyFormat = MapKeyFormats.IntMapKeyFormat
  implicit val ObjectIdMapKeyFormat = MapKeyFormats.ObjectIdMapKeyFormat
  implicit def objectIdSubtypeMapKeyFormat[S <: ObjectId]: MapKeyFormat[S] = MapKeyFormats.objectIdSubtypeMapKeyFormat[S]

}
