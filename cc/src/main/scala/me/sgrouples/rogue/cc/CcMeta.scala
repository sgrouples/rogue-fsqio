package me.sgrouples.rogue.cc

import me.sgrouples.rogue.BsonFormat
import org.bson.{BsonDocument, BsonValue}

trait CcMetaLike[T] {
  type R >: T
}

trait CcMeta[T] extends CcMetaLike[T] {
  //capture T to be able to cast to it
  //type R >: T
  def collectionName: String
  def dba(): com.mongodb.async.client.MongoDatabase

  def dbs(): com.mongodb.client.MongoDatabase

  def read(b: BsonValue): T
  def write(t:T): BsonValue
  def writeR(t:this.R): BsonDocument

  def reader[F](fieldName: String): BsonFormat[F]
}

trait RCcMeta[T] extends CcMeta[T] with BsonFormat[T]{
  def connId = "default"


  override def collectionName = "fakecolname"
  //override def collectionName = classOf[R].getSimpleName.toLowerCase() + "s"

  override def dba(): com.mongodb.async.client.MongoDatabase = CcMongo.getDb(connId).get

  override def dbs(): com.mongodb.client.MongoDatabase = ???

  override def reader[F](fieldName: String): BsonFormat[F] = ???

}