// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue.index

import io.fsq.field.Field
import scala.collection.immutable.ListMap

trait UntypedMongoIndex {
  def asListMap: ListMap[String, Any]

  override def toString() =
    asListMap.map(fld => "%s:%s".format(fld._1, fld._2)).mkString(", ")
}

trait MongoIndex[R] extends UntypedMongoIndex

case class MongoIndex1[R](
  f1: Field[_, R],
  m1: IndexModifier) extends MongoIndex[R] {
  def asListMap = ListMap((f1.name, m1.value))
}

case class MongoIndex2[R](
  f1: Field[_, R],
  m1: IndexModifier,
  f2: Field[_, R],
  m2: IndexModifier) extends MongoIndex[R] {
  def asListMap = ListMap((f1.name, m1.value), (f2.name, m2.value))
}

case class MongoIndex3[R](
  f1: Field[_, R],
  m1: IndexModifier,
  f2: Field[_, R],
  m2: IndexModifier,
  f3: Field[_, R],
  m3: IndexModifier) extends MongoIndex[R] {
  def asListMap = ListMap((f1.name, m1.value), (f2.name, m2.value), (f3.name, m3.value))
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
  def asListMap =
    ListMap(
      (f1.name, m1.value),
      (f2.name, m2.value),
      (f3.name, m3.value),
      (f4.name, m4.value))
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
  def asListMap =
    ListMap(
      (f1.name, m1.value),
      (f2.name, m2.value),
      (f3.name, m3.value),
      (f4.name, m4.value),
      (f5.name, m5.value))
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
  def asListMap =
    ListMap(
      (f1.name, m1.value),
      (f2.name, m2.value),
      (f3.name, m3.value),
      (f4.name, m4.value),
      (f5.name, m5.value),
      (f6.name, m6.value))
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
  def asListMap = ListMap(("$**", "text"))
}

case class MongoTextIndex1[R](
  f1: Field[_, R]) extends MongoTextIndex[R] {
  def asListMap = ListMap((f1.name, "text"))
}

case class MongoTextIndex2[R](
  f1: Field[_, R],
  f2: Field[_, R]) extends MongoTextIndex[R] {
  def asListMap = ListMap((f1.name, "text"), (f2.name, "text"))
}

case class MongoTextIndex3[R](
  f1: Field[_, R],
  f2: Field[_, R],
  f3: Field[_, R]) extends MongoTextIndex[R] {
  def asListMap = ListMap((f1.name, "text"), (f2.name, "text"), (f3.name, "text"))
}

case class MongoTextIndex4[R](
  f1: Field[_, R],
  f2: Field[_, R],
  f3: Field[_, R],
  f4: Field[_, R]) extends MongoTextIndex[R] {
  def asListMap = ListMap((f1.name, "text"), (f2.name, "text"), (f3.name, "text"), (f4.name, "text"))
}

case class MongoTextIndex5[R](
  f1: Field[_, R],
  f2: Field[_, R],
  f3: Field[_, R],
  f4: Field[_, R],
  f5: Field[_, R]) extends MongoTextIndex[R] {
  def asListMap = ListMap((f1.name, "text"), (f2.name, "text"), (f3.name, "text"), (f4.name, "text"), (f5.name, "text"))
}

case class MongoTextIndex6[R](
  f1: Field[_, R],
  f2: Field[_, R],
  f3: Field[_, R],
  f4: Field[_, R],
  f5: Field[_, R],
  f6: Field[_, R]) extends MongoTextIndex[R] {
  def asListMap = ListMap((f1.name, "text"), (f2.name, "text"), (f3.name, "text"), (f4.name, "text"), (f5.name, "text"), (f6.name, "text"))
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
