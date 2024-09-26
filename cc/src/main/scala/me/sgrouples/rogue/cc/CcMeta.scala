package me.sgrouples.rogue.cc

import org.mongodb.scala._
import io.fsq.field.Field
import me.sgrouples.rogue.BsonFormat
import org.bson.{BsonDocument, BsonValue}

trait CcMetaLike[-T] {
  type R
}

trait CcMeta[T] extends CcMetaLike[T] {
  type R = T
  //capture T to be able to cast to it
  //type R >: T
  def collectionName: String

  def read(b: BsonValue): T
  def write(t: T): BsonValue
  //TODO - how to make it play nice with types?
  def writeAnyRef(t: AnyRef): BsonDocument

  def reader(field: Field[_, _]): BsonFormat[_]
}
