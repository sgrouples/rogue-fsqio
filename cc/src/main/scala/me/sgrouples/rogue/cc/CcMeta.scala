package me.sgrouples.rogue.cc

import me.sgrouples.rogue.BsonFormat
import org.bson.BsonDocument

trait CcMetaLike[T] {
  type R >: T
}

trait CcMeta[T] extends CcMetaLike[T]{
  //capture T to be able to cast to it
  //type R >: T
  def dba(): com.mongodb.async.client.MongoDatabase
  def dbs(): com.mongodb.client.MongoDatabase
  def format : BsonFormat[R]
  def reader[F](fieldName: String): BsonFormat[F]
}
