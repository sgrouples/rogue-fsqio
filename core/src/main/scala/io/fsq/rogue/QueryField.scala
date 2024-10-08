// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue
import scala.language.higherKinds
import java.time.{LocalDateTime, ZoneOffset}

import com.mongodb.DBObject
import io.fsq.field.{Field, OptionalField, RequiredField}
import java.util.Date
import java.util.regex.Pattern
import java.time.Instant
import org.bson.BsonNull
import org.bson.types.ObjectId

import scala.util.matching.Regex

object CondOps extends Enumeration {
  type Op = Value
  val Ne = Value("$ne")
  val Lt = Value("$lt")
  val Gt = Value("$gt")
  val LtEq = Value("$lte")
  val GtEq = Value("$gte")
  val In = Value("$in")
  val Nin = Value("$nin")
  val Near = Value("$near")
  val All = Value("$all")
  val Size = Value("$size")
  val Exists = Value("$exists")
  val Type = Value("$type")
  val Mod = Value("$mod")
  val NearSphere = Value("$nearSphere")
  val MaxDistance = Value("$maxDistance")
}

object ModOps extends Enumeration {
  type Op = Value
  val Inc = Value("$inc")
  val Mul = Value("$mul")
  val Set = Value("$set")
  val SetOnInsert = Value("$setOnInsert")
  val Unset = Value("$unset")
  val Push = Value("$push")
  val PushAll = Value("$pushAll")
  val AddToSet = Value("$addToSet")
  val Pop = Value("$pop")
  val Pull = Value("$pull")
  val PullAll = Value("$pullAll")
  val Bit = Value("$bit")
  val Rename = Value("$rename")
  val Min = Value("$min")
  val Max = Value("$max")
  val CurrentDate = Value("$currentDate")
}

object BitOps extends Enumeration {
  type Op = Value
  val And = Value("and")
  val Or = Value("or")
  val Xor = Value("xor")
}

object MongoType extends Enumeration {
  type MongoType = Value
  val Double = Value(1)
  val String = Value(2)
  val Object = Value(3)
  val Array = Value(4)
  val Binary = Value(5)
  val ObjectId = Value(7)
  val Boolean = Value(8)
  val Date = Value(9)
  val Null = Value(10)
  val RegEx = Value(11)
  val JavaScript = Value(13)
  val Symbol = Value(15)
  val Int32 = Value(16)
  val Timestamp = Value(17)
  val Int64 = Value(18)
  val MaxKey = Value(127)
  val MinKey = Value(255)
}

// ********************************************************************************
// *** Query fields
// ********************************************************************************
//
// The types of fields that can be queried, and the particular query operations that each supports
// are defined below.

/** Trait representing a field and all the operations on it.
  *
  * @tparam F
  *   the underlying type of the field
  * @tparam V
  *   the type of values allowed to be compared to the field
  * @tparam DB
  *   the type V is converted into in the BSON representation of the field
  * @tparam M
  *   the type of the owner of the field
  */
abstract class AbstractQueryField[F, V, DB, M](val field: Field[F, M]) {
  def valueToDB(v: V): DB
  def valuesToDB(vs: Iterable[V]): Iterable[DB] = vs.map(valueToDB)

  def eqs(v: V) = EqClause(field.name, valueToDB(v))
  def neqs(v: V) = NeQueryClause(field.name, valueToDB(v))
  def in[L](vs: L)(implicit ev: L => Iterable[V]) =
    QueryHelpers.inListClause(field.name, valuesToDB(ev(vs)))
  def nin[L](vs: L)(implicit ev: L => Iterable[V]) =
    NinQueryClause(field.name, QueryHelpers.validatedList(valuesToDB(ev(vs))))

  def lt(v: V) = LtQueryClause(field.name, valueToDB(v))
  def gt(v: V) = GtQueryClause(field.name, valueToDB(v))
  def lte(v: V) = LtEqQueryClause(field.name, valueToDB(v))

  def gte(v: V) = GtEqQueryClause(field.name, valueToDB(v))

  def <(v: V) = lt(v)
  def <=(v: V) = lte(v)
  def >(v: V) = gt(v)
  def >=(v: V) = gte(v)

  def between(v1: V, v2: V) =
    new BetweenQueryClause(field.name, valueToDB(v1), valueToDB(v2))

  def between(range: (V, V)) =
    new BetweenQueryClause(field.name, valueToDB(range._1), valueToDB(range._2))

  def exists(b: Boolean) = new ExistsQueryClause(field.name, b)
  def hastype(t: MongoType.Value) = new TypeQueryClause(field.name, t)

  def not[F2](clause: Field[F, M] => QueryClause[F2]): QueryClause[F2] = {
    val c = clause(field)
    c.negated = true
    c
  }

  def eqsNull = EqClause(field.name, BsonNull.VALUE)
  def neqsNull = NeQueryClause(field.name, BsonNull.VALUE)
}

class QueryField[V: BSONType, M](field: Field[V, M])
    extends AbstractQueryField[V, V, AnyRef, M](field) {
  override def valueToDB(v: V): AnyRef = BSONType[V].asBSONObject(v)
}

class DateQueryField[M](field: Field[Date, M])
    extends AbstractQueryField[Date, Instant, Date, M](field) {
  override def valueToDB(d: Instant) = new Date(d.toEpochMilli)

  def eqs(d: Date) = EqClause(field.name, d)
  def neqs(d: Date) = new NeQueryClause(field.name, d)

  def between(d1: Date, d2: Date) =
    new BetweenQueryClause(field.name, d1, d2)

  def before(d: Date) = new LtQueryClause(field.name, d)
  def after(d: Date) = new GtQueryClause(field.name, d)
  def onOrBefore(d: Date) = new LtEqQueryClause(field.name, d)
  def onOrAfter(d: Date) = new GtEqQueryClause(field.name, d)

  def before(d: Instant) =
    new LtQueryClause(field.name, new Date(d.toEpochMilli))
  def after(d: Instant) =
    new GtQueryClause(field.name, new Date(d.toEpochMilli))
  def onOrBefore(d: Instant) =
    new LtEqQueryClause(field.name, new Date(d.toEpochMilli))
  def onOrAfter(d: Instant) =
    new GtEqQueryClause(field.name, new Date(d.toEpochMilli))
}

class DateTimeQueryField[M](field: Field[Instant, M])
    extends AbstractQueryField[Instant, Instant, Date, M](field) {
  override def valueToDB(d: Instant) = new Date(d.toEpochMilli)

  def before(d: Instant) =
    new LtQueryClause(field.name, new Date(d.toEpochMilli))
  def after(d: Instant) =
    new GtQueryClause(field.name, new Date(d.toEpochMilli))
  def onOrBefore(d: Instant) =
    new LtEqQueryClause(field.name, new Date(d.toEpochMilli))
  def onOrAfter(d: Instant) =
    new GtEqQueryClause(field.name, new Date(d.toEpochMilli))
}

class EnumNameQueryField[M, V <: Enumeration#Value](field: Field[V, M])
    extends AbstractQueryField[V, V, String, M](field) {
  override def valueToDB(e: V) = e.toString
}

class EnumIdQueryField[M, V](field: Field[V, M], conv: V => Int)
    extends AbstractQueryField[V, V, Int, M](field) {
  override def valueToDB(e: V) = conv(e)
}

class EnumeratumEnumQueryField[M, E <: enumeratum.EnumEntry](field: Field[E, M])
    extends AbstractQueryField[E, E, String, M](field) {
  override def valueToDB(e: E) = e.entryName
}

class GeoQueryField[M](field: Field[LatLong, M])
    extends AbstractQueryField[LatLong, LatLong, java.util.List[Double], M](
      field
    ) {
  override def valueToDB(ll: LatLong) =
    QueryHelpers.list(List(ll.lat, ll.long))

  def eqs(lat: Double, lng: Double) =
    EqClause(field.name, QueryHelpers.list(List(lat, lng)))

  def neqs(lat: Double, lng: Double) =
    new NeQueryClause(field.name, QueryHelpers.list(List(lat, lng)))

  def near(lat: Double, lng: Double, radius: Degrees) =
    new NearQueryClause(
      field.name,
      QueryHelpers.list(List(lat, lng, QueryHelpers.radius(radius)))
    )

  def nearSphere(lat: Double, lng: Double, radians: Radians) =
    new NearSphereQueryClause(field.name, lat, lng, radians)

  def withinCircle(lat: Double, lng: Double, radius: Degrees) =
    new WithinCircleClause(field.name, lat, lng, QueryHelpers.radius(radius))

  def withinBox(lat1: Double, lng1: Double, lat2: Double, lng2: Double) =
    new WithinBoxClause(field.name, lat1, lng1, lat2, lng2)
}

class NumericQueryField[V, M](field: Field[V, M])
    extends AbstractQueryField[V, V, V, M](field) {
  def mod(by: Int, eq: Int) =
    new ModQueryClause(field.name, QueryHelpers.list(List(by, eq)))
  override def valueToDB(v: V) = v
}

class ObjectIdQueryField[F <: ObjectId, M](override val field: Field[F, M])
    extends NumericQueryField(field) {

  private[this] def objId(i: Instant): ObjectId = {
    // copied from ObjectId.legacyToBytes
    val timestamp = i.getEpochSecond
    val bytes = new Array[Byte](12)
    bytes(0) = (timestamp >> 24).toByte
    bytes(1) = (timestamp >> 16).toByte
    bytes(2) = (timestamp >> 8).toByte
    bytes(3) = timestamp.toByte
    new ObjectId(bytes)
  }

  def before(d: Instant) =
    new LtQueryClause(field.name, objId(d))

  def after(d: Instant) =
    new GtQueryClause(field.name, objId(d))

  def between(d1: Instant, d2: Instant) =
    new StrictBetweenQueryClause(field.name, objId(d1), objId(d2))

  def between(range: (Instant, Instant)) =
    new StrictBetweenQueryClause(field.name, objId(range._1), objId(range._2))

  def before(d: LocalDateTime) =
    new LtQueryClause(field.name, localDateTimeToOid(d))

  def after(d: LocalDateTime) =
    new GtQueryClause(field.name, localDateTimeToOid(d))

  def between(d1: LocalDateTime, d2: LocalDateTime) =
    new StrictBetweenQueryClause(
      field.name,
      localDateTimeToOid(d1),
      localDateTimeToOid(d2)
    )

  //different name because of the type erasure - can't dispatch by inner type
  def betweenR(range: (LocalDateTime, LocalDateTime)) =
    new StrictBetweenQueryClause(
      field.name,
      localDateTimeToOid(range._1),
      localDateTimeToOid(range._2)
    )

  private def localDateTimeToOid(d: LocalDateTime): ObjectId = objId(
    d.toInstant(ZoneOffset.UTC)
  )
}

class ForeignObjectIdQueryField[F <: ObjectId, M, T](
    override val field: Field[F, M],
    val getId: T => F
) extends ObjectIdQueryField[F, M](field) {
  // The implicit parameter is solely to get around the fact that because of
  // erasure, this method and the method in AbstractQueryField look the same.
  def eqs(obj: T)(implicit ev: T =:= T) =
    EqClause(field.name, getId(obj))

  // The implicit parameter is solely to get around the fact that because of
  // erasure, this method and the method in AbstractQueryField look the same.
  def neqs(obj: T)(implicit ev: T =:= T) =
    new NeQueryClause(field.name, getId(obj))

  // The implicit parameter is solely to get around the fact that because of
  // erasure, this method and the method in AbstractQueryField look the same.
  def in(objs: Iterable[T])(implicit ev: T =:= T) =
    QueryHelpers.inListClause(field.name, objs.map(getId))

  // The implicit parameter is solely to get around the fact that because of
  // erasure, this method and the method in AbstractQueryField look the same.
  def nin(objs: Iterable[T])(implicit ev: T =:= T) =
    new NinQueryClause(field.name, QueryHelpers.validatedList(objs.map(getId)))
}

trait StringRegexOps[V, M] {
  self: AbstractQueryField[V, _ <: String, _ <: String, M] =>

  def startsWith(s: String): RegexQueryClause[PartialIndexScan] =
    new RegexQueryClause[PartialIndexScan](
      field.name,
      PartialIndexScan,
      Pattern.compile("^" + Pattern.quote(s))
    )

  def matches(p: Pattern): RegexQueryClause[DocumentScan] =
    new RegexQueryClause[DocumentScan](field.name, DocumentScan, p)

  def matches(r: Regex): RegexQueryClause[DocumentScan] =
    matches(r.pattern)

  def regexWarningNotIndexed(p: Pattern) =
    matches(p)
}

class StringQueryField[F <: String, M](override val field: Field[F, M])
    extends AbstractQueryField[F, F, F, M](field)
    with StringRegexOps[F, M] {

  override def valueToDB(v: F): F = v
}

class CaseClassQueryField[V, M](val field: Field[V, M]) {
  def unsafeField[F](name: String): SelectableDummyField[F, M] = {
    new SelectableDummyField[F, M](field.name + "." + name, field.owner)
  }
}

class BsonRecordQueryField[M, B](
    field: Field[B, M],
    asDBObject: B => DBObject,
    defaultValue: B
) extends AbstractQueryField[B, B, DBObject, M](field) {
  override def valueToDB(b: B) = asDBObject(b)

  def subfield[V](subfield: B => Field[V, B]): SelectableDummyField[V, M] = {
    new SelectableDummyField[V, M](
      field.name + "." + subfield(defaultValue).name,
      field.owner
    )
  }

  def unsafeField[V](name: String): DummyField[V, M] = {
    new DummyField[V, M](field.name + "." + name, field.owner)
  }

  def subselect[V](f: B => Field[V, B]): SelectableDummyField[V, M] = subfield(
    f
  )
}

abstract class AbstractSeqQueryField[F, V, DB, M, CC[_] <: Seq[_]](
    field: Field[CC[F], M]
) extends AbstractQueryField[CC[F], V, DB, M](field) {

  def all(vs: Iterable[V]) =
    QueryHelpers.allListClause(field.name, valuesToDB(vs))

  def eqs(vs: Iterable[V]) =
    EqClause(field.name, QueryHelpers.validatedList(valuesToDB(vs)))

  def neqs(vs: Iterable[V]) =
    new NeQueryClause(field.name, QueryHelpers.validatedList(valuesToDB(vs)))

  def size(s: Int) =
    new SizeQueryClause(field.name, s)

  def contains(v: V) =
    EqClause(field.name, valueToDB(v))

  def notcontains(v: V) =
    new NeQueryClause(field.name, valueToDB(v))

  def at(i: Int): DummyField[V, M] =
    new DummyField[V, M](field.name + "." + i.toString, field.owner)

  def idx(i: Int): DummyField[V, M] = at(i)
}

abstract class AbstractArrayQueryField[F, V, DB, M, X](
    field: Field[Array[X], M]
) extends AbstractQueryField[Array[X], V, DB, M](field) {

  def all(vs: Iterable[V]) =
    QueryHelpers.allListClause(field.name, valuesToDB(vs))

  def eqs(vs: Iterable[V]) =
    EqClause(field.name, QueryHelpers.validatedList(valuesToDB(vs)))

  def neqs(vs: Iterable[V]) =
    new NeQueryClause(field.name, QueryHelpers.validatedList(valuesToDB(vs)))

  def size(s: Int) =
    new SizeQueryClause(field.name, s)

  def contains(v: V) =
    EqClause(field.name, valueToDB(v))

  def notcontains(v: V) =
    new NeQueryClause(field.name, valueToDB(v))

  def at(i: Int): DummyField[V, M] =
    new DummyField[V, M](field.name + "." + i.toString, field.owner)

  def idx(i: Int): DummyField[V, M] = at(i)
}

/*
AbstractSeqQueryField[F, V, DB, M, CC[_] <: Seq[_]]
 */
class SeqQueryField[V: BSONType, M, ST[_] <: Seq[_]](field: Field[ST[V], M])
    extends AbstractSeqQueryField[V, V, AnyRef, M, ST](field) {
  override def valueToDB(v: V): AnyRef = BSONType[V].asBSONObject(v)
}

class StringsSeqQueryField[M, CC[_] <: Seq[_]](
    override val field: Field[CC[String], M]
) extends AbstractSeqQueryField[String, String, String, M, CC](field)
    with StringRegexOps[CC[String], M] {

  override def valueToDB(v: String) = v
}

class BsonRecordSeqQueryField[M, B](
    field: Field[List[B], M],
    rec: B,
    asDBObject: B => DBObject
) extends AbstractSeqQueryField[B, B, DBObject, M, List](field) {
  override def valueToDB(b: B) = asDBObject(b)

  def subfield[V, V1](
      f: B => Field[V, B]
  )(implicit ev: Rogue.Flattened[V, V1]): SelectableDummyField[List[V1], M] = {
    new SelectableDummyField[List[V1], M](
      field.name + "." + f(rec).name,
      field.owner
    )
  }

  def subselect[V, V1](
      f: B => Field[V, B]
  )(implicit ev: Rogue.Flattened[V, V1]): SelectField[Option[List[V1]], M] = {
    Rogue.roptionalFieldToSelectField(subfield(f))
  }

  def unsafeField[V](name: String): DummyField[V, M] = {
    new DummyField[V, M](field.name + "." + name, field.owner)
  }

  def elemMatch[V](clauseFuncs: (B => QueryClause[_])*) = {
    new ElemMatchWithPredicateClause(field.name, clauseFuncs.map(cf => cf(rec)))
  }
}

class CaseClassSeqQueryField[M, B](
    field: Field[List[B], M],
    asDBObject: B => DBObject
) extends AbstractSeqQueryField[B, B, DBObject, M, List](field) {
  override def valueToDB(b: B) = asDBObject(b)

  /*def subfield[V, V1](f: B => Field[V, B])(implicit ev: Rogue.Flattened[V, V1]): SelectableDummyField[List[V1], M] = {
    new SelectableDummyField[List[V1], M](field.name + "." + f(rec).name, field.owner)
  }

  def subselect[V, V1](f: B => Field[V, B])(implicit ev: Rogue.Flattened[V, V1]): SelectField[Option[List[V1]], M] = {
    Rogue.roptionalFieldToSelectField(subfield(f))
  }

  def unsafeField[V](name: String): DummyField[V, M] = {
    new DummyField[V, M](field.name + "." + name, field.owner)
  }

  def elemMatch[V](clauseFuncs: (B => QueryClause[_])*) = {
    new ElemMatchWithPredicateClause(
      field.name,
      clauseFuncs.map(cf => cf(rec))
    )
  }*/
}

class MapQueryField[V, M](val field: Field[Map[String, V], M]) {
  def at(key: String): SelectableDummyField[V, M] = {
    new SelectableDummyField(field.name + "." + key, field.owner)
  }
}

class EnumerationSeqQueryField[V <: Enumeration#Value, M, CC[_] <: Seq[_]](
    field: Field[CC[V], M]
) extends AbstractSeqQueryField[V, V, String, M, CC](field) {
  override def valueToDB(v: V) = v.toString
}

// ********************************************************************************
// *** Modify fields
// ********************************************************************************

class SafeModifyField[V, M](val field: Field[V, M]) {
  def unset = new ModifyClause(ModOps.Unset, field.name -> 1)
  def rename(newName: String) =
    new ModifyClause(ModOps.Rename, field.name -> newName)
}

abstract class AbstractModifyField[V, DB, M](val field: Field[V, M]) {
  def valueToDB(v: V): DB
  def setTo(v: V): ModifyClause =
    new ModifyClause(ModOps.Set, field.name -> valueToDB(v))
  def setTo(vOpt: Option[V]): ModifyClause = vOpt match {
    case Some(v) => setTo(v)
    case none    => new SafeModifyField(field).unset
  }

  def setOnInsertTo(v: V): ModifyClause =
    new ModifyClause(ModOps.SetOnInsert, field.name -> valueToDB(v))

  def min(v: V) = new ModifyClause(ModOps.Min, field.name -> valueToDB(v))
  def max(v: V) = new ModifyClause(ModOps.Max, field.name -> valueToDB(v))
}

class ModifyField[V: BSONType, M](field: Field[V, M])
    extends AbstractModifyField[V, AnyRef, M](field) {
  override def valueToDB(v: V): AnyRef = BSONType[V].asBSONObject(v)
}

class DateModifyField[M](field: Field[Date, M])
    extends AbstractModifyField[Date, Date, M](field) {
  override def valueToDB(d: Date) = d

  def setTo(d: Instant) =
    new ModifyClause(ModOps.Set, field.name -> new Date(d.toEpochMilli))
  def setOnInsertTo(d: Instant): ModifyClause =
    new ModifyClause(ModOps.SetOnInsert, field.name -> new Date(d.toEpochMilli))
  def min(d: Instant) =
    new ModifyClause(ModOps.Min, field.name -> new Date(d.toEpochMilli))
  def max(d: Instant) =
    new ModifyClause(ModOps.Max, field.name -> new Date(d.toEpochMilli))

  def currentDate = new ModifyClause(ModOps.CurrentDate, field.name -> true)
}

class DateTimeModifyField[M](field: Field[Instant, M])
    extends AbstractModifyField[Instant, Date, M](field) {
  override def valueToDB(d: Instant) = new Date(d.toEpochMilli)

  def currentDate = new ModifyClause(ModOps.CurrentDate, field.name -> true)
}

class EnumerationModifyField[M, E <: Enumeration#Value](field: Field[E, M])
    extends AbstractModifyField[E, String, M](field) {
  override def valueToDB(e: E) = e.toString
}

class GeoModifyField[M](field: Field[LatLong, M])
    extends AbstractModifyField[LatLong, java.util.List[Double], M](field) {
  override def valueToDB(ll: LatLong) =
    QueryHelpers.list(List(ll.lat, ll.long))

  def setTo(lat: Double, long: Double) =
    new ModifyClause(
      ModOps.Set,
      field.name -> QueryHelpers.list(List(lat, long))
    )
}

class NumericModifyField[V, M](override val field: Field[V, M])
    extends AbstractModifyField[V, V, M](field) {
  override def valueToDB(v: V): V = v

  def inc(v: Int) = new ModifyClause(ModOps.Inc, field.name -> v)
  def inc(v: Long) = new ModifyClause(ModOps.Inc, field.name -> v)
  def inc(v: Double) = new ModifyClause(ModOps.Inc, field.name -> v)
  def mul(v: Int) = new ModifyClause(ModOps.Mul, field.name -> v)
  def mul(v: Long) = new ModifyClause(ModOps.Mul, field.name -> v)
  def mul(v: Double) = new ModifyClause(ModOps.Mul, field.name -> v)

  def bitAnd(v: Int) = new ModifyBitClause(field.name, v, BitOps.And)
  def bitOr(v: Int) = new ModifyBitClause(field.name, v, BitOps.Or)
  def bitXor(v: Int) = new ModifyBitClause(field.name, v, BitOps.Xor)
}

class BsonRecordModifyField[M, B](field: Field[B, M], asDBObject: B => DBObject)
    extends AbstractModifyField[B, DBObject, M](field) {
  override def valueToDB(b: B) = asDBObject(b)
}

class MapModifyField[V, M](field: Field[Map[String, V], M])
    extends AbstractModifyField[Map[String, V], java.util.Map[String, V], M](
      field
    ) {
  override def valueToDB(m: Map[String, V]) = QueryHelpers.makeJavaMap(m)
}

abstract class AbstractSeqModifyField[V, DB, M, CC[_] <: Seq[_]](
    val field: Field[CC[V], M]
) {
  def valueToDB(v: V): DB

  def valuesToDB(vs: Iterable[V]) = vs.map(valueToDB _)

  def setTo(vs: Iterable[V]) =
    new ModifyClause(
      ModOps.Set,
      field.name -> QueryHelpers.list(valuesToDB(vs))
    )
  def push(v: V) =
    new ModifyClause(ModOps.Push, field.name -> valueToDB(v))

  def push(vs: Iterable[V]) =
    new ModifyPushEachClause(field.name, valuesToDB(vs))

  def push(vs: Iterable[V], slice: Int) =
    new ModifyPushEachSliceClause(field.name, slice, valuesToDB(vs))

  def push(vs: Iterable[V], slice: Int, position: Int) =
    new ModifyPushEachSlicePositionClause(
      field.name,
      slice,
      position,
      valuesToDB(vs)
    )

  def pushAll(vs: Iterable[V]) =
    new ModifyClause(
      ModOps.PushAll,
      field.name -> QueryHelpers.list(valuesToDB(vs))
    )

  def addToSet(v: V) =
    new ModifyClause(ModOps.AddToSet, field.name -> valueToDB(v))

  def addToSet(vs: Iterable[V]) =
    new ModifyAddEachClause(field.name, valuesToDB(vs))

  def popFirst =
    new ModifyClause(ModOps.Pop, field.name -> -1)

  def popLast =
    new ModifyClause(ModOps.Pop, field.name -> 1)

  def pull(v: V) =
    new ModifyClause(ModOps.Pull, field.name -> valueToDB(v))

  def pullAll(vs: Iterable[V]) =
    new ModifyClause(
      ModOps.PullAll,
      field.name -> QueryHelpers.list(valuesToDB(vs))
    )

  def pullWhere(clauseFuncs: (Field[V, M] => QueryClause[_])*) =
    new ModifyPullWithPredicateClause(
      field.name,
      clauseFuncs.map(cf => cf(new DummyField[V, M](field.name, field.owner)))
    )

  def $ : Field[V, M] = {
    new SelectableDummyField[V, M](field.name + ".$", field.owner)
  }
}

class SeqModifyField[V: BSONType, M, CC[_] <: Seq[_]](field: Field[CC[V], M])
    extends AbstractSeqModifyField[V, AnyRef, M, CC](field) {
  override def valueToDB(v: V): AnyRef = BSONType[V].asBSONObject(v)
}

class ArrayModifyField[V: BSONType, M](field: Field[Array[V], M])
    extends AbstractArrayModifyField[V, AnyRef, M](field) {
  override def valueToDB(v: V): AnyRef = BSONType[V].asBSONObject(v)
}

class CaseClassSeqModifyField[V, M, CC[_] <: Seq[_]](
    field: Field[CC[V], M],
    asDBObject: V => DBObject
) extends AbstractSeqModifyField[V, DBObject, M, CC](field) {
  override def valueToDB(v: V) = asDBObject(v)
  //QueryHelpers.asDBObject(v)
}

abstract class AbstractArrayModifyField[V, DB, M](
    val field: Field[Array[V], M]
) {
  def valueToDB(v: V): DB

  def valuesToDB(vs: Iterable[V]) = vs.map(valueToDB _)

  def setTo(vs: Iterable[V]) =
    new ModifyClause(
      ModOps.Set,
      field.name -> QueryHelpers.list(valuesToDB(vs))
    )
  def push(v: V) =
    new ModifyClause(ModOps.Push, field.name -> valueToDB(v))

  def push(vs: Iterable[V]) =
    new ModifyPushEachClause(field.name, valuesToDB(vs))

  def push(vs: Iterable[V], slice: Int) =
    new ModifyPushEachSliceClause(field.name, slice, valuesToDB(vs))

  def push(vs: Iterable[V], slice: Int, position: Int) =
    new ModifyPushEachSlicePositionClause(
      field.name,
      slice,
      position,
      valuesToDB(vs)
    )

  def pushAll(vs: Iterable[V]) =
    new ModifyClause(
      ModOps.PushAll,
      field.name -> QueryHelpers.list(valuesToDB(vs))
    )

  def addToSet(v: V) =
    new ModifyClause(ModOps.AddToSet, field.name -> valueToDB(v))

  def addToSet(vs: Iterable[V]) =
    new ModifyAddEachClause(field.name, valuesToDB(vs))

  def popFirst =
    new ModifyClause(ModOps.Pop, field.name -> -1)

  def popLast =
    new ModifyClause(ModOps.Pop, field.name -> 1)

  def pull(v: V) =
    new ModifyClause(ModOps.Pull, field.name -> valueToDB(v))

  def pullAll(vs: Iterable[V]) =
    new ModifyClause(
      ModOps.PullAll,
      field.name -> QueryHelpers.list(valuesToDB(vs))
    )

  def pullWhere(clauseFuncs: (Field[V, M] => QueryClause[_])*) =
    new ModifyPullWithPredicateClause(
      field.name,
      clauseFuncs.map(cf => cf(new DummyField[V, M](field.name, field.owner)))
    )

  def $ : Field[V, M] = {
    new SelectableDummyField[V, M](field.name + ".$", field.owner)
  }
}

class EnumerationSeqModifyField[V <: Enumeration#Value, M, CC[_] <: Seq[_]](
    field: Field[CC[V], M]
) extends AbstractSeqModifyField[V, String, M, CC](field) {
  override def valueToDB(v: V) = v.toString
}

class BsonRecordSeqModifyField[M, B](
    field: Field[List[B], M],
    rec: B,
    asDBObject: B => DBObject
)(implicit mf: Manifest[B])
    extends AbstractSeqModifyField[B, DBObject, M, List](field) {
  override def valueToDB(b: B) = asDBObject(b)

  // override def $: BsonRecordField[M, B] = {
  //   new BsonRecordField[M, B](field.owner, rec.meta)(mf) {
  //     override def name = field.name + ".$"
  //   }
  // }

  def pullObjectWhere(clauseFuncs: (B => QueryClause[_])*) = {
    new ModifyPullObjWithPredicateClause(
      field.name,
      clauseFuncs.map(cf => cf(rec))
    )
  }
}

// ********************************************************************************
// *** Select fields
// ********************************************************************************

/** Fields that can be turned into SelectFields can be used in a .select call.
  *
  * This class is sealed because only RequiredFields and OptionalFields should
  * be selectable. Be careful when adding subclasses of this class.
  */
sealed abstract class SelectField[V, M](
    val field: Field[_, M],
    val slc: Option[(Int, Option[Int])] = None
) {
  // Input will be a Box of the value, and output will either be a Box of the value or the value itself
  def valueOrDefault(v: Option[_]): Any
  def slice(s: Int): SelectField[V, M]
  def slice(s: Int, e: Int): SelectField[V, M]
  def $$ : SelectField[V, M]
}

final class MandatorySelectField[V, M](
    override val field: RequiredField[V, M],
    override val slc: Option[(Int, Option[Int])] = None
) extends SelectField[V, M](field, slc) {
  override def valueOrDefault(v: Option[_]): Any =
    v.getOrElse(field.defaultValue)
  override def slice(s: Int): MandatorySelectField[V, M] = {
    new MandatorySelectField(field, Some((s, None)))
  }
  override def slice(s: Int, e: Int): MandatorySelectField[V, M] = {
    new MandatorySelectField(field, Some((s, Some(e))))
  }
  def $$ : MandatorySelectField[V, M] = {
    val fld = new RequiredDummyField[V, M](
      field.name + ".$",
      field.owner,
      field.defaultValue
    )
    new MandatorySelectField(fld, slc)
  }
}

final class OptionalSelectField[V, M](
    override val field: OptionalField[V, M],
    override val slc: Option[(Int, Option[Int])] = None
) extends SelectField[Option[V], M](field, slc) {
  override def valueOrDefault(v: Option[_]): Any = {
    v.map {
      case innV: Some[_] => innV.get
      case innV          => innV
    }
  }
  override def slice(s: Int): OptionalSelectField[V, M] = {
    new OptionalSelectField(field, Some((s, None)))
  }
  override def slice(s: Int, e: Int): OptionalSelectField[V, M] = {
    new OptionalSelectField(field, Some((s, Some(e))))
  }
  def $$ : OptionalSelectField[V, M] = {
    val fld = new SelectableDummyField[V, M](field.name + ".$", field.owner)
    new OptionalSelectField(fld, slc)
  }
}

// ********************************************************************************
// *** Dummy field
// ********************************************************************************

class DummyField[V, R](override val name: String, override val owner: R)
    extends Field[V, R]

class SelectableDummyField[V, R](
    override val name: String,
    override val owner: R
) extends OptionalField[V, R]

class RequiredDummyField[V, R](
    override val name: String,
    override val owner: R,
    override val defaultValue: V
) extends RequiredField[V, R]
