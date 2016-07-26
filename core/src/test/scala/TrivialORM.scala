// Copyright 2015 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue.test

import com.mongodb.{ BasicDBObjectBuilder, DBObject }
import io.fsq.field.OptionalField
import org.bson.Document

/**
 * A trivial ORM layer that implements the interfaces rogue needs. The goal is
 * to make sure that rogue-core works without the assistance of rogue-lift.
 * Ideally this would be even smaller; as it is, I needed to copy-paste some
 * code from the Lift implementations.
 */
object TrivialORM {

  trait Record {
    type Self >: this.type <: Record

    def meta: Meta[Self]
  }

  trait Meta[R] {
    def collectionName: String

    def fromDBObject(dbo: DBObject): R

    def toDBObject(record: R): DBObject

    def fromDocument(doc: Document): R

    def toDocument(record: R): Document
  }

}

case class SimpleRecord(a: Int, b: String) extends TrivialORM.Record {
  override type Self = SimpleRecord

  override def meta: SimpleRecord.type = SimpleRecord
}

object SimpleRecord extends TrivialORM.Meta[SimpleRecord] {
  val a = new OptionalField[Int, SimpleRecord.type] {
    override val owner = SimpleRecord;
    override val name = "a"
  }
  val b = new OptionalField[String, SimpleRecord.type] {
    override val owner = SimpleRecord;
    override val name = "b"
  }

  override val collectionName = "simple_records"

  override def fromDBObject(dbo: DBObject): SimpleRecord = {
    new SimpleRecord(dbo.get(a.name).asInstanceOf[Int], dbo.get(b.name).asInstanceOf[String])
  }

  override def toDBObject(record: SimpleRecord): DBObject = {
    (BasicDBObjectBuilder
      .start
      .add(a.name, record.a)
      .add(b.name, record.b)
      .get)
  }

  override def fromDocument(doc: Document): SimpleRecord = {
    new SimpleRecord(doc.get(a.name).asInstanceOf[Int], doc.get(b.name).asInstanceOf[String])
  }

  override def toDocument(record: SimpleRecord): Document = {
    new Document(a.name, record.a).append(b.name, record.b)
  }
}