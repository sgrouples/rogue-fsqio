// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue.index

import io.fsq.field.Field
import org.bson.{ BsonDocument, BsonString }

import scala.collection.immutable.ListMap

trait UntypedMongoIndex {
  def asBsonDocument: BsonDocument

  override def toString() = asBsonDocument.toString
}

trait MongoIndex[R] extends UntypedMongoIndex

case class MongoIndex1[R](
  f1: Field[_, R],
  m1: IndexModifier) extends MongoIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument().append(f1.name, m1.bsonValue)
}

case class MongoIndex2[R](
  f1: Field[_, R],
  m1: IndexModifier,
  f2: Field[_, R],
  m2: IndexModifier) extends MongoIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument()
    .append(f1.name, m1.bsonValue)
    .append(f2.name, m2.bsonValue)
}

case class MongoIndex3[R](
  f1: Field[_, R],
  m1: IndexModifier,
  f2: Field[_, R],
  m2: IndexModifier,
  f3: Field[_, R],
  m3: IndexModifier) extends MongoIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument()
    .append(f1.name, m1.bsonValue)
    .append(f2.name, m2.bsonValue)
    .append(f3.name, m3.bsonValue)
}

case class MongoIndex4[R](
  f1: Field[_, R],
  m1: IndexModifier,
  f2: Field[_, R],
  m2: IndexModifier,
  f3: Field[_, R],
  m3: IndexModifier,
  f4: Field[_, R],
  m4: IndexModifier) extends MongoIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument()
    .append(f1.name, m1.bsonValue)
    .append(f2.name, m2.bsonValue)
    .append(f3.name, m3.bsonValue)
    .append(f4.name, m4.bsonValue)
}

case class MongoIndex5[R](
  f1: Field[_, R],
  m1: IndexModifier,
  f2: Field[_, R],
  m2: IndexModifier,
  f3: Field[_, R],
  m3: IndexModifier,
  f4: Field[_, R],
  m4: IndexModifier,
  f5: Field[_, R],
  m5: IndexModifier) extends MongoIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument()
    .append(f1.name, m1.bsonValue)
    .append(f2.name, m2.bsonValue)
    .append(f3.name, m3.bsonValue)
    .append(f4.name, m4.bsonValue)
    .append(f5.name, m5.bsonValue)
}

case class MongoIndex6[R](
  f1: Field[_, R],
  m1: IndexModifier,
  f2: Field[_, R],
  m2: IndexModifier,
  f3: Field[_, R],
  m3: IndexModifier,
  f4: Field[_, R],
  m4: IndexModifier,
  f5: Field[_, R],
  m5: IndexModifier,
  f6: Field[_, R],
  m6: IndexModifier) extends MongoIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument()
    .append(f1.name, m1.bsonValue)
    .append(f2.name, m2.bsonValue)
    .append(f3.name, m3.bsonValue)
    .append(f4.name, m4.bsonValue)
    .append(f5.name, m5.bsonValue)
    .append(f6.name, m6.bsonValue)
}

trait IndexBuilder[M] {
  def index(
    f1: Field[_, M],
    m1: IndexModifier): MongoIndex1[M] =
    MongoIndex1[M](f1, m1)

  def index(
    f1: Field[_, M],
    m1: IndexModifier,
    f2: Field[_, M],
    m2: IndexModifier): MongoIndex2[M] =
    MongoIndex2[M](f1, m1, f2, m2)

  def index(
    f1: Field[_, M],
    m1: IndexModifier,
    f2: Field[_, M],
    m2: IndexModifier,
    f3: Field[_, M],
    m3: IndexModifier): MongoIndex3[M] =
    MongoIndex3[M](f1, m1, f2, m2, f3, m3)

  def index(
    f1: Field[_, M],
    m1: IndexModifier,
    f2: Field[_, M],
    m2: IndexModifier,
    f3: Field[_, M],
    m3: IndexModifier,
    f4: Field[_, M],
    m4: IndexModifier): MongoIndex4[M] =
    MongoIndex4[M](f1, m1, f2, m2, f3, m3, f4, m4)

  def index(
    f1: Field[_, M],
    m1: IndexModifier,
    f2: Field[_, M],
    m2: IndexModifier,
    f3: Field[_, M],
    m3: IndexModifier,
    f4: Field[_, M],
    m4: IndexModifier,
    f5: Field[_, M],
    m5: IndexModifier): MongoIndex5[M] =
    MongoIndex5[M](f1, m1, f2, m2, f3, m3, f4, m4, f5, m5)

  def index(
    f1: Field[_, M],
    m1: IndexModifier,
    f2: Field[_, M],
    m2: IndexModifier,
    f3: Field[_, M],
    m3: IndexModifier,
    f4: Field[_, M],
    m4: IndexModifier,
    f5: Field[_, M],
    m5: IndexModifier,
    f6: Field[_, M],
    m6: IndexModifier): MongoIndex6[M] =
    MongoIndex6[M](f1, m1, f2, m2, f3, m3, f4, m4, f5, m5, f6, m6)
}

trait MongoTextIndex[R] extends UntypedMongoIndex

case class MongoTextIndexAll[R]() extends MongoTextIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument().append("$**", new BsonString("text"))
}

case class MongoTextIndex1[R](
  f1: Field[_, R]) extends MongoTextIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument().append(f1.name, new BsonString("text"))
}

case class MongoTextIndex2[R](
  f1: Field[_, R],
  f2: Field[_, R]) extends MongoTextIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument()
    .append(f1.name, new BsonString("text"))
    .append(f2.name, new BsonString("text"))
}

case class MongoTextIndex3[R](
  f1: Field[_, R],
  f2: Field[_, R],
  f3: Field[_, R]) extends MongoTextIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument()
    .append(f1.name, new BsonString("text"))
    .append(f2.name, new BsonString("text"))
    .append(f3.name, new BsonString("text"))
}

case class MongoTextIndex4[R](
  f1: Field[_, R],
  f2: Field[_, R],
  f3: Field[_, R],
  f4: Field[_, R]) extends MongoTextIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument()
    .append(f1.name, new BsonString("text"))
    .append(f2.name, new BsonString("text"))
    .append(f3.name, new BsonString("text"))
    .append(f4.name, new BsonString("text"))
}

case class MongoTextIndex5[R](
  f1: Field[_, R],
  f2: Field[_, R],
  f3: Field[_, R],
  f4: Field[_, R],
  f5: Field[_, R]) extends MongoTextIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument()
    .append(f1.name, new BsonString("text"))
    .append(f2.name, new BsonString("text"))
    .append(f3.name, new BsonString("text"))
    .append(f4.name, new BsonString("text"))
    .append(f5.name, new BsonString("text"))
}

case class MongoTextIndex6[R](
  f1: Field[_, R],
  f2: Field[_, R],
  f3: Field[_, R],
  f4: Field[_, R],
  f5: Field[_, R],
  f6: Field[_, R]) extends MongoTextIndex[R] {
  def asBsonDocument: BsonDocument = new BsonDocument()
    .append(f1.name, new BsonString("text"))
    .append(f2.name, new BsonString("text"))
    .append(f3.name, new BsonString("text"))
    .append(f4.name, new BsonString("text"))
    .append(f5.name, new BsonString("text"))
    .append(f6.name, new BsonString("text"))
}

case class TextIndexBuilder[M](rec: M) {
  def textIndex(): MongoTextIndexAll[M] =
    MongoTextIndexAll[M]()

  def textIndex(
    f1: Field[_, M]): MongoTextIndex1[M] =
    MongoTextIndex1[M](f1)

  def textIndex(
    f1: Field[_, M],
    f2: Field[_, M]): MongoTextIndex2[M] =
    MongoTextIndex2[M](f1, f2)

  def textIndex(
    f1: Field[_, M],
    f2: Field[_, M],
    f3: Field[_, M]): MongoTextIndex3[M] =
    MongoTextIndex3[M](f1, f2, f3)

  def textIndex(
    f1: Field[_, M],
    f2: Field[_, M],
    f3: Field[_, M],
    f4: Field[_, M]): MongoTextIndex4[M] =
    MongoTextIndex4[M](f1, f2, f3, f4)

  def textIndex(
    f1: Field[_, M],
    f2: Field[_, M],
    f3: Field[_, M],
    f4: Field[_, M],
    f5: Field[_, M]): MongoTextIndex5[M] =
    MongoTextIndex5[M](f1, f2, f3, f4, f5)

  def textIndex(
    f1: Field[_, M],
    f2: Field[_, M],
    f3: Field[_, M],
    f4: Field[_, M],
    f5: Field[_, M],
    f6: Field[_, M]): MongoTextIndex6[M] =
    MongoTextIndex6[M](f1, f2, f3, f4, f5, f6)
}
