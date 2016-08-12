package me.sgrouples.rogue.cc

/**
 * Created by mar on 12.07.2016.
 */

// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.

import java.time.{ Instant, LocalDateTime }

import io.fsq.field.{ RequiredField, Field => RField, OptionalField => ROptionalField }
import io.fsq.rogue.{ BSONType, FindAndModifyQuery, LatLong, ListModifyField, ListQueryField, MandatorySelectField, MapModifyField, MapQueryField, ModifyField, ModifyQuery, NumericModifyField, NumericQueryField, ObjectIdQueryField, OptionalSelectField, Query, QueryField, QueryHelpers, Rogue, RogueException, SafeModifyField, SelectField, ShardingOk, StringQueryField, StringsListQueryField, Unlimited, Unordered, Unselected, Unskipped, _ }
import io.fsq.rogue.MongoHelpers.AndCondition
import io.fsq.rogue.index.IndexBuilder
import java.util.{ Date, UUID }

import me.sgrouples.rogue._
import org.bson.types.ObjectId

trait CcRogue {
  def OrQuery[M, R](subqueries: Query[M, R, _]*): Query[M, R, Unordered with Unselected with Unlimited with Unskipped with HasOrClause] = {
    subqueries.toList match {
      case Nil => throw new RogueException("No subqueries supplied to OrQuery", null)
      case q :: qs => {
        val orCondition = QueryHelpers.orConditionFromQueries(q :: qs)
        Query[M, R, Unordered with Unselected with Unlimited with Unskipped with HasOrClause](
          q.meta, q.collectionName, None, None, None, None, None,
          AndCondition(Nil, Some(orCondition)), None, None, None
        )
      }
    }
  }

  /* Following are a collection of implicit conversions which take a meta-record and convert it to
   * a QueryBuilder. This allows users to write queries as "QueryType where ...".
   */
  implicit def ccMetaToQueryBuilder[M <: CcMeta[_], R](meta: M with CcMeta[R]): Query[M, R, InitialState] =
    Query[M, R, InitialState](
      meta, meta.collectionName, None, None, None, None, None, AndCondition(Nil, None), None, None, None
    )

  implicit def metaRecordToIndexBuilder[M <: CcMeta[_]](meta: M): IndexBuilder[M] =
    IndexBuilder(meta)

  implicit def ccMetaToInsertQuery[MB <: CcMeta[_], M <: MB, R, State](meta: M): InsertableQuery[MB, M, R, InitialState] = {
    val query = Query[M, R, InitialState](
      meta, meta.collectionName, None, None, None, None, None, AndCondition(Nil, None), None, None, None
    )
    InsertableQuery(query, CcBsonExecutors).asInstanceOf[InsertableQuery[MB, M, R, InitialState]]
  }

  implicit def queryToCcQuery[MB <: CcMeta[_], M <: MB, R, State](query: Query[M, R, State])(implicit ev: ShardingOk[M, State]): ExecutableQuery[CcMeta[_], M, R, State] = {
    ExecutableQuery(
      query,
      CcBsonExecutors
    )
  }

  //}

  /*
  case class ExecutableModifyQuery[MB, M <: MB, State](query: ModifyQuery[M, State],
                                                         dba: AsyncBsonQueryExecutor[MB]) {
*/

  implicit def modifyQueryToCCModifyQuery[MB <: CcMeta[_], M <: MB, R, State](
    query: ModifyQuery[M, State]
  ): ExecutableModifyQuery[CcMeta[_], M, State] = {
    ExecutableModifyQuery(
      query,
      CcBsonExecutors
    )
  }

  implicit def findAndModifyQueryToCcAndModifyQuery[M <: CcMeta[_], R](
    query: FindAndModifyQuery[M, R]
  ): ExecutableFindAndModifyQuery[CcMeta[_], M, R] = {
    ExecutableFindAndModifyQuery(
      query,
      CcBsonExecutors
    )
  }

  implicit def metaRecordToCcQuery[MB <: CcMeta[_], M <: MB, R](meta: M): ExecutableQuery[MB, M, R, InitialState] = {
    val queryBuilder = Query[M, R, InitialState](
      meta, meta.collectionName, None, None, None, None, None, AndCondition(Nil, None), None, None, None
    )

    val ccQuery = queryToCcQuery(queryBuilder)
    ccQuery.asInstanceOf[ExecutableQuery[MB, M, R, InitialState]]
  }

  /*
  MB <: CcMetaLike[R], M <: MB,  R, State](
                                                       query: Query[M, R, State],
                                                       dba: AsyncBsonQueryExecutor[MB]
                                                     )(implicit ev: ShardingOk[M, State]) {

   */

  /*implicit def bsonRecordFieldToBsonRecordQueryField[
  M <: BsonRecord[M],
  B <: BsonRecord[B]
  ](
     f: BsonRecordField[M, B]
   ): BsonRecordQueryField[M, B] = {
    val rec = f.defaultValue // a hack to get at the embedded record
    new BsonRecordQueryField[M, B](f, _.asDBObject, rec)
  }

  implicit def rbsonRecordFieldToBsonRecordQueryField[
  M <: BsonRecord[M],
  B <: BsonRecord[B]
  ](
     f: RField[B, M]
   ): BsonRecordQueryField[M, B] = {
    // a hack to get at the embedded record
    val owner = f.owner
    if (f.name.indexOf('.') >= 0) {
      val fieldName = f.name.takeWhile(_ != '.')
      val field = owner.fieldByName(fieldName).openOr(sys.error("Error getting field "+fieldName+" for "+owner))
      val typedField = field.asInstanceOf[BsonRecordListField[M, B]]
      // a gross hack to get at the embedded record
      val rec: B = typedField.setFromJValue(JArray(JInt(0) :: Nil)).openOrThrowException("a gross hack to get at the embedded record").head
      new BsonRecordQueryField[M, B](f, _.asDBObject, rec)
    } else {
      val fieldName = f.name
      val field = owner.fieldByName(fieldName).openOr(sys.error("Error getting field "+fieldName+" for "+owner))
      val typedField = field.asInstanceOf[BsonRecordField[M, B]]
      val rec: B = typedField.defaultValue
      new BsonRecordQueryField[M, B](f, _.asDBObject, rec)
    }
  }

  implicit def bsonRecordListFieldToBsonRecordListQueryField[
  M <: BsonRecord[M],
  B <: BsonRecord[B]
  ](f: BsonRecordListField[M, B]): BsonRecordListQueryField[M, B] = {
    val rec = f.setFromJValue(JArray(JInt(0) :: Nil)).openOrThrowException("a gross hack to get at the embedded record").head
    new BsonRecordListQueryField[M, B](f, rec, _.asDBObject)
  }
*/
  implicit def localDateTimeFieldToLocalDateTimeQueryField[O <: CcMeta[_]](f: RField[LocalDateTime, O]): LocalDateTimeQueryField[O] =
    new LocalDateTimeQueryField(f)

  implicit def instantFieldToInstantQueryField[O <: CcMeta[_]](f: RField[Instant, O]): InstantQueryField[O] =
    new InstantQueryField(f)

  implicit def caseClassFieldToQueryField[C, M <: CcMeta[C], O](f: CClassField[C, M, O]): CClassQueryField[C, M, O] =
    new CClassQueryField[C, M, O](f, f.owner)

  implicit def optCaseClassFieldToQueryField[C, M <: CcMeta[C], O](f: OptCClassField[C, M, O]): OptCClassQueryField[C, M, O] =
    new OptCClassQueryField[C, M, O](f, f.owner)

  implicit def selectableDummyFieldToQueryField[C, M <: CcMeta[C], O](f: SelectableDummyCCField[C, M, O]): CClassLikeQueryField[C, M, O] = {
    new CClassLikeQueryField[C, M, O](f, f.meta, f.owner)
  }

  implicit def ccListFieldToListQueryField[C, M <: CcMeta[C], O](f: CClassListField[C, M, O]): CClassSeqQueryField[C, M, O] = new CClassSeqQueryField[C, M, O](f, f.owner)
  /*
  //(field: Field[List[B], M], rec: B, toBson: B => BsonValue)
  implicit def doubleFieldtoNumericQueryField[M <: BsonRecord[M], F]
  (f: Field[Double, M]): NumericQueryField[Double, M] =
    new NumericQueryField(f)

  implicit def enumFieldToEnumNameQueryField[M <: BsonRecord[M], F <: Enumeration#Value]
  (f: Field[F, M]): EnumNameQueryField[M, F] =
    new EnumNameQueryField(f)

  implicit def enumFieldToEnumQueryField[M <: BsonRecord[M], F <: Enumeration]
  (f: EnumField[M, F]): EnumIdQueryField[M, F#Value] =
    new EnumIdQueryField(f)

  implicit def enumerationListFieldToEnumerationListQueryField[M <: BsonRecord[M], F <: Enumeration#Value]
  (f: Field[List[F], M]): EnumerationListQueryField[F, M] =
    new EnumerationListQueryField[F, M](f)


  implicit def intFieldtoNumericQueryField[M <: BsonRecord[M], F](f: Field[Int, M]): NumericQueryField[Int, M] =
    new NumericQueryField(f)

  implicit def latLongFieldToGeoQueryField[M <: BsonRecord[M]](f: Field[LatLong, M]): GeoQueryField[M] =
    new GeoQueryField(f)

  implicit def listFieldToListQueryField[M <: BsonRecord[M], F: BSONType](f: Field[List[F], M]): ListQueryField[F, M] =
    new ListQueryField[F, M](f)

  implicit def stringsListFieldToStringsListQueryField[M <: BsonRecord[M]](f: Field[List[String], M]): StringsListQueryField[M] =
    new StringsListQueryField[M](f)

  implicit def longFieldtoNumericQueryField[M <: BsonRecord[M], F <: Long](f: Field[F, M]): NumericQueryField[F, M] =
    new NumericQueryField(f)

  implicit def objectIdFieldToObjectIdQueryField[M <: BsonRecord[M], F <: ObjectId](f: Field[F, M]): ObjectIdQueryField[F, M] =
    new ObjectIdQueryField(f)

  implicit def mapFieldToMapQueryField[M, F](f: RField[Map[String, F], M]): MapQueryField[F, M] =
    new MapQueryField[F, M](f)

  implicit def stringFieldToStringQueryField[F <: String, M <: BsonRecord[M]](f: Field[F, M]): StringQueryField[F, M] =
    new StringQueryField(f)

  // ModifyField implicits
  implicit def fieldToModifyField[M <: BsonRecord[M], F: BSONType](f: Field[F, M]): ModifyField[F, M] = new ModifyField(f)
  implicit def fieldToSafeModifyField[M <: BsonRecord[M], F](f: Field[F, M]): SafeModifyField[F, M] = new SafeModifyField(f)
*/
  implicit def ccFieldToCcModifyField[C, M <: CcMeta[C], O](f: CClassField[C, M, O]): CClassModifyField[C, M, O] =
    new CClassModifyField[C, M, O](f)

  implicit def optCcFieldToCcModifyField[C, M <: CcMeta[C], O](f: OptCClassField[C, M, O]): OptCClassModifyField[C, M, O] =
    new OptCClassModifyField[C, M, O](f)

  //implicit def localDateTimeFieldToLocalDateTimeQueryField[O <: CcMeta[_]](f: RField[LocalDateTime, O]): LocalDateTimeQueryField[O] =
  //  new LocalDateTimeQueryField(f)

  implicit def uuidFieldToQueryField[O <: CcMeta[_]](f: RField[UUID, O]): QueryField[UUID, O] = new QueryField(f)

  /*

implicit def optCcFieldToCcModifyField[C <: Product, M <: CcMeta[C], O]
(f: OptCClassField[C, M, O]): OptCClassModifyField[C, M, O] =
  new OptCClassModifyField[C, M, O](f)
*/

  implicit def ccListFieldToCCSeqModifyField[C, M <: CcMeta[C], O](f: CClassListField[C, M, O]): CClassSeqModifyField[C, M, O] = new CClassSeqModifyField[C, M, O](f)

  /*implicit def bsonRecordListFieldToBsonRecordListModifyField[
M <: BsonRecord[M],
B <: BsonRecord[B]
](
   f: BsonRecordListField[M, B]
 )(
   implicit mf: Manifest[B]
 ): BsonRecordListModifyField[M, B] = {
  val rec = f.setFromJValue(JArray(JInt(0) :: Nil)).openOrThrowException("a gross hack to get at the embedded record").head
  new BsonRecordListModifyField[M, B](f, rec, _.asDBObject)(mf)
}*/

  implicit def localDateTimeFieldToLocalDateTimeModifyField[O <: CcMeta[_]](f: RField[LocalDateTime, O]): LocalDateTimeModifyField[O] =
    new LocalDateTimeModifyField(f)

  implicit def instantFieldToLocalDateTimeModifyField[O <: CcMeta[_]](f: RField[Instant, O]): InstantModifyField[O] =
    new InstantModifyField(f)

  //implicit def datetimeRFieldToDateModifyField[M](f: RField[DateTime, M]): DateTimeModifyField[M] = new DateTimeModifyField(f)

  /*
  implicit def ccListFieldToListModifyField[M <: BsonRecord[M], V]
  (f: MongoCaseClassListField[M, V]): CaseClassListModifyField[V, M] =
    new CaseClassListModifyField[V, M](liftField2Recordv2Field(f))

  implicit def doubleFieldToNumericModifyField[M <: BsonRecord[M]]
  (f: Field[Double, M]): NumericModifyField[Double, M] =
    new NumericModifyField(f)

  implicit def enumerationFieldToEnumerationModifyField[M <: BsonRecord[M], F <: Enumeration#Value]
  (f: Field[F, M]): EnumerationModifyField[M, F] =
    new EnumerationModifyField(f)

  implicit def enumerationListFieldToEnumerationListModifyField[M <: BsonRecord[M], F <: Enumeration#Value]
  (f: Field[List[F], M]): EnumerationListModifyField[F, M] =
    new EnumerationListModifyField[F, M](f)

  implicit def intFieldToIntModifyField[M <: BsonRecord[M]]
  (f: Field[Int, M]): NumericModifyField[Int, M] =
    new NumericModifyField(f)

  implicit def latLongFieldToGeoQueryModifyField[M <: BsonRecord[M]](f: Field[LatLong, M]): GeoModifyField[M] =
    new GeoModifyField(f)

  implicit def listFieldToListModifyField[M <: BsonRecord[M], F: BSONType](f: Field[List[F], M]): ListModifyField[F, M] =
    new ListModifyField[F, M](f)

  implicit def longFieldToNumericModifyField[M <: BsonRecord[M]](f: Field[Long, M]): NumericModifyField[Long, M] =
    new NumericModifyField(f)

  implicit def mapFieldToMapModifyField[M <: BsonRecord[M], F](f: Field[Map[String, F], M]): MapModifyField[F, M] =
    new MapModifyField[F, M](f)
*/
  // SelectField implicits

  /*
  /Users/mar/git/rogue-fsqio/lift/src/test/scala/EndToEndAsyncTest.scala:169:
  applied implicit conversion from x$100.claims.type to ?{def $$: ?} = implicit def mandatoryFieldToSelectField[M <: net.liftweb.mongodb.record.BsonRecord[M], V](f: net.liftweb.record.Field[V,M] with net.liftweb.record.MandatoryTypedField[V]): io.fsq.rogue.SelectField[V,M]ESC[0m
   */
  implicit def mandatoryFieldToSelectField[M, V](f: MCField[V, M]): SelectField[V, M] =
    new MandatorySelectField(f)

  implicit def optionalFieldToSelectField[M <: CcMeta[_], V](f: OCField[V, M]): SelectField[Option[V], M] =
    new OptionalSelectField(new ROptionalField[V, M] {
      override def name = f.name
      override def owner = f.owner
    })

  /*
  implicit def mandatoryLiftField2RequiredRecordv2Field[M <: BsonRecord[M], V](
                                                                                f: Field[V, M] with MandatoryTypedField[V]
                                                                              ): io.fsq.field.RequiredField[V, M] = new io.fsq.field.RequiredField[V, M] {
    override def name = f.name
    override def owner = f.owner
    override def defaultValue = f.defaultValue
  }
*/
  /*
  class BsonRecordIsBSONType[T <: BsonRecord[T]] extends BSONType[T] {
    override def asBSONObject(v: T): AnyRef = v.asDBObject
  }

  object _BsonRecordIsBSONType extends BsonRecordIsBSONType[Nothing]

  implicit def BsonRecordIsBSONType[T <: BsonRecord[T]]: BSONType[T] = _BsonRecordIsBSONType.asInstanceOf[BSONType[T]]
}*/
  implicit val localDateIsFlattened = new Rogue.Flattened[LocalDateTime, LocalDateTime]

  implicit val instantIsFlattend = new Rogue.Flattened[Instant, Instant]
}

object CcRogue extends Rogue with CcRogue
