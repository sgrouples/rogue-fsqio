package me.sgrouples.rogue.cc

import io.fsq.field.Field
import me.sgrouples.rogue.BsonFormat
import me.sgrouples.rogue.naming.{ LowerCase, NamingStrategy }
import org.bson.{ BsonDocument, BsonValue }

import scala.reflect.ClassTag

trait CcMetaLike[-T] {
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
  def write(t: T): BsonValue
  //TODO - how to make it play nice with types?
  def writeAnyRef(t: AnyRef): BsonDocument

  def reader(field: Field[_, _]): BsonFormat[_]
}

class RCcMeta[T](collName: String)(implicit f: BsonFormat[T]) extends CcMeta[T] {

  def this(namingStrategy: NamingStrategy = LowerCase)(implicit f: BsonFormat[T], classTag: ClassTag[T]) {
    this(namingStrategy[T])
  }

  def connId = "lift" //default connection identifier is "lift" for backwards compat with lift-rogue

  override def collectionName: String = collName

  override def dba(): com.mongodb.async.client.MongoDatabase = CcMongo.getDb(connId).get

  override def dbs(): com.mongodb.client.MongoDatabase = CcMongo.getDbSync(connId).get

  override def reader(field: Field[_, _]): BsonFormat[_] = {
    val fieldName = field.name.replaceAll("\\.\\$", "")
    // if field.isInstanceOf[]
    val r = f.flds.get(fieldName)
    r.orElse(starReader(fieldName)).getOrElse {
      throw new RuntimeException(s"No reader for field ${fieldName}, available keys ${f.flds.keys.mkString(",")}")
    }
  }

  //special case for Map formats
  private[this] def starReader(fieldName: String): Option[BsonFormat[_]] = {
    val i = fieldName.lastIndexOf('.')
    if (i > 0) {
      val newName = fieldName.substring(0, i + 1) + "*"
      f.flds.get(newName)
    } else None
  }

  override def read(b: BsonValue): T = f.read(b)

  override def write(t: T): BsonValue = f.write(t)

  override def writeAnyRef(t: AnyRef): BsonDocument = f.write(t.asInstanceOf[T]).asDocument()

}
