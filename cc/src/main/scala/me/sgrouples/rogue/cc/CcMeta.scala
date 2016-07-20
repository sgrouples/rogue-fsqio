package me.sgrouples.rogue.cc

import io.fsq.field.Field
import me.sgrouples.rogue.{BsonFormat, BsonFormats}
import org.bson.{BsonDocument, BsonValue}

trait CcMetaLike[T] {
  type R
}

trait CcMeta[T] extends CcMetaLike[T] {
  type R = T
  //capture T to be able to cast to it
  //type R >: T
  def collectionName: String
  def dba(): com.mongodb.async.client.MongoDatabase

  def dbs(): com.mongodb.client.MongoDatabase

  def read(b: BsonValue): T
  def write(t:T): BsonValue
  //TODO - how to make it play nice with types?
  def writeAnyRef(t:AnyRef): BsonDocument

  def reader(field: Field[_,_]): BsonFormat[_]
}

class RCcMeta[T](collName: String)(implicit f:BsonFormat[T]) extends CcMeta[T]{

  def connId = "default"

  override def collectionName: String = collName
  //I want : classOf[R].getSimpleName.toLowerCase() + "s", but don't know how

  override def dba(): com.mongodb.async.client.MongoDatabase = CcMongo.getDb(connId).get

  override def dbs(): com.mongodb.client.MongoDatabase = ???

  override def reader(field: Field[_,_]): BsonFormat[_] = {
    val fieldName = field.name
   // if field.isInstanceOf[]
    val r = f.flds.get(fieldName)
    r.getOrElse{
      throw new RuntimeException(s"No reader for field ${fieldName}, avaialble keys ${f.flds.keys.mkString(",")}")
    }
  }

  override def read(b: BsonValue): T = f.read(b)

  override def write(t: T): BsonValue = f.write(t)

  override def writeAnyRef(t: AnyRef): BsonDocument = f.write(t.asInstanceOf[T]).asDocument()

}
