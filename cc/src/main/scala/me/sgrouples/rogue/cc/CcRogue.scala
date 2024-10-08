package me.sgrouples.rogue.cc

/** Created by mar on 12.07.2016.
  */

// Copyright 2016 Sgrouples Inc. All Rights Reserved.
//

import enumeratum.EnumEntry

import java.time.{Instant, LocalDateTime}
import io.fsq.field.{Field => RField, OptionalField => ROptionalField}
import io.fsq.rogue.{
  FindAndModifyQuery,
  MandatorySelectField,
  ModifyQuery,
  OptionalSelectField,
  Query,
  QueryField,
  QueryHelpers,
  Rogue,
  RogueException,
  SelectField,
  ShardingOk,
  Unlimited,
  Unordered,
  Unselected,
  Unskipped,
  _
}
import io.fsq.rogue.MongoHelpers.AndCondition

import java.util.{Currency, Locale, UUID}
import me.sgrouples.rogue._
import org.bson.types.ObjectId
import org.mongodb.scala.result.{
  DeleteResult,
  InsertManyResult,
  InsertOneResult,
  UpdateResult
}

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait CcRogue {
  def OrQuery[M, R](subqueries: Query[M, R, _]*): Query[
    M,
    R,
    Unordered with Unselected with Unlimited with Unskipped with HasOrClause
  ] = {
    subqueries.toList match {
      case Nil =>
        throw new RogueException("No subqueries supplied to OrQuery", null)
      case q :: qs => {
        val orCondition = QueryHelpers.orConditionFromQueries(q :: qs)
        Query[
          M,
          R,
          Unordered with Unselected with Unlimited with Unskipped with HasOrClause
        ](
          q.meta,
          q.collectionName,
          None,
          None,
          None,
          None,
          None,
          AndCondition(Nil, Some(orCondition), None),
          None,
          None,
          None
        )
      }
    }
  }

  /* Following are a collection of implicit conversions which take a meta-record and convert it to
   * a QueryBuilder. This allows users to write queries as "QueryType where ...".
   */
  implicit def ccMetaToQueryBuilder[M <: CcMeta[_], R](
      meta: M with CcMeta[R]
  ): Query[M, R, InitialState] =
    Query[M, R, InitialState](
      meta,
      meta.collectionName,
      None,
      None,
      None,
      None,
      None,
      AndCondition(Nil, None, None),
      None,
      None,
      None
    )

  implicit def ccMetaToInsertQuery[MB <: CcMeta[_], M <: MB, R, State](
      meta: M
  ): InsertableQuery[MB, M, R, InitialState] = {
    val query = Query[M, R, InitialState](
      meta,
      meta.collectionName,
      None,
      None,
      None,
      None,
      None,
      AndCondition(Nil, None, None),
      None,
      None,
      None
    )
    InsertableQuery(query, CcBsonExecutors)
      .asInstanceOf[InsertableQuery[MB, M, R, InitialState]]
  }

  implicit def queryToCcQuery[MB <: CcMeta[_], M <: MB, R, State](
      query: Query[M, R, State]
  )(implicit
      ev: ShardingOk[M, State]
  ): ExecutableQuery[CcMeta[_], M, R, State] = {
    ExecutableQuery(query, CcBsonExecutors)
  }

  implicit def modifyQueryToCCModifyQuery[MB <: CcMeta[_], M <: MB, R, State](
      query: ModifyQuery[M, State]
  ): ExecutableModifyQuery[CcMeta[_], M, State] = {
    ExecutableModifyQuery(query, CcBsonExecutors)
  }

  implicit def findAndModifyQueryToCcAndModifyQuery[M <: CcMeta[_], R](
      query: FindAndModifyQuery[M, R]
  ): ExecutableFindAndModifyQuery[CcMeta[_], M, R] = {
    ExecutableFindAndModifyQuery(query, CcBsonExecutors)
  }

  implicit def ccMetaToAggregateQuery[MB <: CcMeta[_], M <: MB, State](
      meta: M
  ): AggregateQuery[MB, M, InitialState] =
    AggregateQuery(meta.collectionName, CcBsonExecutors)

  implicit def metaRecordToCcQuery[MB <: CcMeta[_], M <: MB, R](
      meta: M
  ): ExecutableQuery[MB, M, R, InitialState] = {
    val queryBuilder = Query[M, R, InitialState](
      meta,
      meta.collectionName,
      None,
      None,
      None,
      None,
      None,
      AndCondition(Nil, None, None),
      None,
      None,
      None
    )

    val ccQuery = queryToCcQuery(queryBuilder)
    ccQuery.asInstanceOf[ExecutableQuery[MB, M, R, InitialState]]
  }

  //given Conversion[]

  implicit def localDateTimeFieldToLocalDateTimeQueryField[O <: CcMeta[_]](
      f: RField[LocalDateTime, O]
  ): LocalDateTimeQueryField[O] =
    new LocalDateTimeQueryField(f)

  implicit def instantFieldToInstantQueryField[O <: CcMeta[_]](
      f: RField[Instant, O]
  ): InstantQueryField[O] =
    new InstantQueryField(f)

  implicit def currencyFieldToCurrencyQueryField[O <: CcMeta[_]](
      f: RField[Currency, O]
  ): CurrencyQueryField[O] =
    new CurrencyQueryField[O](f)

  implicit def localeFieldToLocaleQueryField[O <: CcMeta[_]](
      f: RField[Locale, O]
  ): LocaleQueryField[O] =
    new LocaleQueryField[O](f)

  implicit def bigDecimalFieldToCurrencyQueryField[O <: CcMeta[_]](
      f: RField[BigDecimal, O]
  ): BigDecimalQueryField[O] =
    new BigDecimalQueryField[O](f)

  implicit def caseClassFieldToQueryField[C, M <: CcMeta[C], O](
      f: CClassField[C, M, O]
  ): CClassQueryField[C, M, O] =
    new CClassQueryField[C, M, O](f, f.owner)

  implicit def optCaseClassFieldToQueryField[C, M <: CcMeta[C], O](
      f: OptCClassField[C, M, O]
  ): OptCClassQueryField[C, M, O] =
    new OptCClassQueryField[C, M, O](f, f.owner)

  implicit def selectableDummyFieldToQueryField[C, M <: CcMeta[C], O](
      f: SelectableDummyCCField[C, M, O]
  ): CClassLikeQueryField[C, M, O] = {
    new CClassLikeQueryField[C, M, O](f, f.meta, f.owner)
  }

  implicit def ccListFieldToListQueryField[C, M <: CcMeta[C], O](
      f: CClassListField[C, M, O]
  ): CClassSeqQueryField[C, M, O, List] =
    new CClassSeqQueryField[C, M, O, List](f, f.owner)

  implicit def optCCListFieldToListQueryField[C, M <: CcMeta[C], O](
      f: OptCClassListField[C, M, O]
  ): CClassSeqQueryField[C, M, O, List] =
    new CClassSeqQueryField[C, M, O, List](f, f.owner)

  implicit def ccArrayFieldToListQueryField[C, M <: CcMeta[C], O](
      f: CClassArrayField[C, M, O]
  ): CClassArrayQueryField[C, M, O] =
    new CClassArrayQueryField[C, M, O](f, f.owner)

  implicit def optCcArrayFieldToListQueryField[C, M <: CcMeta[C], O](
      f: OptCClassArrayField[C, M, O]
  ): CClassArrayQueryField[C, M, O] =
    new CClassArrayQueryField[C, M, O](f, f.owner)

  implicit def ccFieldToCcModifyField[C, M <: CcMeta[C], O](
      f: CClassField[C, M, O]
  ): CClassModifyField[C, M, O] =
    new CClassModifyField[C, M, O](f)

  implicit def optCcFieldToCcModifyField[C, M <: CcMeta[C], O](
      f: OptCClassField[C, M, O]
  ): OptCClassModifyField[C, M, O] =
    new OptCClassModifyField[C, M, O](f)

  implicit def uuidFieldToQueryField[O <: CcMeta[_]](
      f: RField[UUID, O]
  ): QueryField[UUID, O] = new QueryField(f)

  implicit def ccListFieldToCCSeqModifyField[C, M <: CcMeta[C], O](
      f: CClassListField[C, M, O]
  ): CClassSeqModifyField[C, M, O, List] =
    new CClassSeqModifyField[C, M, O, List](f)

  implicit def optCListFieldToCCSeqModifyField[C, M <: CcMeta[C], O](
      f: OptCClassListField[C, M, O]
  ): CClassSeqModifyField[C, M, O, List] =
    new CClassSeqModifyField[C, M, O, List](f)

  implicit def ccArrayFieldToCCArrayModifyField[C, M <: CcMeta[C], O](
      f: CClassArrayField[C, M, O]
  ): CClassArrayModifyField[C, M, O] = new CClassArrayModifyField[C, M, O](f)

  given [C, M <: CcMeta[C], O]
      : Conversion[CClassArrayField[C, M, O], CClassArrayModifyField[C, M, O]]
    with
    def apply(f: CClassArrayField[C, M, O]): CClassArrayModifyField[C, M, O] =
      new CClassArrayModifyField[C, M, O](f)

  implicit def optCcArrayFieldToCCArrayModifyField[C, M <: CcMeta[C], O](
      f: OptCClassArrayField[C, M, O]
  ): CClassArrayModifyField[C, M, O] = new CClassArrayModifyField[C, M, O](f)

  given [C, M <: CcMeta[C], O]: Conversion[
    OptCClassArrayField[C, M, O],
    CClassArrayModifyField[C, M, O]
  ] with
    def apply(
        f: OptCClassArrayField[C, M, O]
    ): CClassArrayModifyField[C, M, O] =
      new CClassArrayModifyField[C, M, O](f)

  implicit def localDateTimeFieldToLocalDateTimeModifyField[O <: CcMeta[_]](
      f: RField[LocalDateTime, O]
  ): LocalDateTimeModifyField[O] =
    new LocalDateTimeModifyField(f)

  implicit def instantFieldToLocalDateTimeModifyField[O <: CcMeta[_]](
      f: RField[Instant, O]
  ): InstantModifyField[O] =
    new InstantModifyField(f)

  implicit def currencyFieldToCurrencyModifyField[O <: CcMeta[_]](
      f: RField[Currency, O]
  ): CurrencyModifyField[O] =
    new CurrencyModifyField[O](f)

  implicit def bigDecimalFieldToCurrencyModifyField[O <: CcMeta[_]](
      f: RField[BigDecimal, O]
  ): BigDecimalModifyField[O] =
    new BigDecimalModifyField[O](f)

  implicit def localeFieldToLocaleModifyField[O <: CcMeta[_]](
      f: RField[Locale, O]
  ): LocaleModifyField[O] =
    new LocaleModifyField[O](f)

  implicit def mandatoryFieldToSelectField[M, V](
      f: MCField[V, M]
  ): SelectField[V, M] =
    new MandatorySelectField(f)

  implicit def optionalFieldToSelectField[M <: CcMeta[_], V](
      f: OCField[V, M]
  ): SelectField[Option[V], M] =
    new OptionalSelectField(new ROptionalField[V, M] {
      override def name = f.name
      override def owner = f.owner
    })

  implicit def enumIdFieldToEnumQueryField[O <: CcMeta[
    _
  ], E <: Enumeration#Value](
      f: EnumIdField[E, O]
  ): EnumIdQueryField[O, E] = {
    new EnumIdQueryField(
      f.asInstanceOf[io.fsq.field.Field[E, O]],
      (e: E) => e.id
    )
  }
  // this is here to force proper implicit resolution

  implicit def optRnumIdFieldToEnumQueryField[O <: CcMeta[
    _
  ], E <: Enumeration#Value](
      f: OptEnumIdField[E, O]
  ): EnumIdQueryField[O, E] =
    new EnumIdQueryField(
      f.asInstanceOf[io.fsq.field.Field[E, O]],
      (e: E) => e.id
    )

  implicit def enumIdFieldToEnumIdModifyField[O <: CcMeta[
    _
  ], E <: Enumeration#Value](
      f: EnumIdField[E, O]
  ): EnumIdModifyField[O, E] =
    new EnumIdModifyField(
      f.asInstanceOf[io.fsq.field.Field[E, O]],
      (e: E) => e.id
    )

  // this is here to force proper implicit resolution

  implicit def optEnumIdFieldToEnumIdModifyField[O <: CcMeta[
    _
  ], E <: Enumeration#Value](
      f: OptEnumIdField[E, O]
  ): EnumIdModifyField[O, E] =
    new EnumIdModifyField(
      f.asInstanceOf[io.fsq.field.Field[E, O]],
      (e: E) => e.id
    )

  given localDateIsFlattened: Rogue.Flattened[LocalDateTime, LocalDateTime] =
    new Rogue.Flattened[LocalDateTime, LocalDateTime]

  given instantIsFlattend: Rogue.Flattened[Instant, Instant] =
    new Rogue.Flattened[Instant, Instant]

  implicit def objIdSubtypeIsFlattened[T <: ObjectId]
      : Rogue.Flattened[T, ObjectId] =
    new Rogue.Flattened[T, ObjectId]

  implicit def binaryFieldToQueryField[M](
      f: RField[Array[Byte], M]
  ): BinaryQueryField[M] = new BinaryQueryField(f)

  implicit def binaryFieldToModifyField[M](
      f: RField[Array[Byte], M]
  ): BinaryModifyField[M] = new BinaryModifyField(f)

  implicit def updateResultToVoid(i: Future[UpdateResult]): Future[Unit] =
    i.map(_ => ())(scala.concurrent.ExecutionContext.parasitic)
  implicit def deleteResultToVoid(
      i: Future[com.mongodb.client.result.DeleteResult]
  ): Future[Unit] =
    i.map(_ => ())(scala.concurrent.ExecutionContext.parasitic)
  implicit def insertManyResultToVoid(
      i: Future[InsertManyResult]
  ): Future[Unit] =
    i.map(_ => ())(scala.concurrent.ExecutionContext.parasitic)
  implicit def insertOneResultToVoid(i: Future[InsertOneResult]): Future[Unit] =
    i.map(_ => ())(scala.concurrent.ExecutionContext.parasitic)

  implicit def enumeratumFieldToEnumeratumEnumQueryField[M, E <: EnumEntry](
      f: EnumeratumField[E, M]
  ): EnumeratumEnumQueryField[M, E] =
    new EnumeratumEnumQueryField(f)
}

object CcRogue extends Rogue with CcRogue
