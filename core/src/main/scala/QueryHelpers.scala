// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue

import com.mongodb.WriteConcern
import io.fsq.rogue.index.UntypedMongoIndex
import scala.Iterable

case class Degrees(value: Double)
case class Radians(value: Double)
case class LatLong(lat: Double, long: Double)

object QueryHelpers {

  trait QueryLogger {
    def log(query: Query[_, _, _], instanceName: String, msg: => String, timeMillis: Long): Unit
    def onExecuteQuery[T](query: Query[_, _, _], instanceName: String, msg: => String, func: => T): T
    def onExecuteWriteCommand[T](operationName: String, collectionName: String, instanceName: String, msg: => String, func: => T): T
    def logIndexMismatch(query: Query[_, _, _], msg: => String): Unit
    def logIndexHit(query: Query[_, _, _], index: UntypedMongoIndex): Unit
    def warn(query: Query[_, _, _], msg: => String): Unit
  }

  class DefaultQueryLogger extends QueryLogger {
    override def log(query: Query[_, _, _], instanceName: String, msg: => String, timeMillis: Long): Unit = {}
    override def onExecuteQuery[T](query: Query[_, _, _], instanceName: String, msg: => String, func: => T): T = func
    override def onExecuteWriteCommand[T](operationName: String, collectionName: String, instanceName: String, msg: => String, func: => T): T = func
    override def logIndexMismatch(query: Query[_, _, _], msg: => String): Unit = {}
    override def logIndexHit(query: Query[_, _, _], index: UntypedMongoIndex): Unit = {}
    override def warn(query: Query[_, _, _], msg: => String): Unit = {}
  }

  object NoopQueryLogger extends DefaultQueryLogger

  var logger: QueryLogger = NoopQueryLogger

  trait QueryValidator {
    def validateList[T](xs: Iterable[T]): Unit
    def validateRadius(d: Degrees): Degrees
    def validateQuery[M](query: Query[M, _, _], indexes: Option[Seq[UntypedMongoIndex]]): Unit
    def validateModify[M](modify: ModifyQuery[M, _], indexes: Option[Seq[UntypedMongoIndex]]): Unit
    def validateFindAndModify[M, R](modify: FindAndModifyQuery[M, R], indexes: Option[Seq[UntypedMongoIndex]]): Unit
  }

  class DefaultQueryValidator extends QueryValidator {
    override def validateList[T](xs: Iterable[T]): Unit = {}
    override def validateRadius(d: Degrees) = d
    override def validateQuery[M](query: Query[M, _, _], indexes: Option[Seq[UntypedMongoIndex]]): Unit = {}
    override def validateModify[M](modify: ModifyQuery[M, _], indexes: Option[Seq[UntypedMongoIndex]]): Unit = {} // todo possibly validate for update without upsert, yet setOnInsert present -- ktoso
    override def validateFindAndModify[M, R](modify: FindAndModifyQuery[M, R], indexes: Option[Seq[UntypedMongoIndex]]): Unit = {}
  }

  object NoopQueryValidator extends DefaultQueryValidator

  var validator: QueryValidator = NoopQueryValidator

  trait QueryTransformer {
    def transformQuery[M](query: Query[M, _, _]): Query[M, _, _]
    def transformModify[M](modify: ModifyQuery[M, _]): ModifyQuery[M, _]
    def transformFindAndModify[M, R](modify: FindAndModifyQuery[M, R]): FindAndModifyQuery[M, R]
  }

  class DefaultQueryTransformer extends QueryTransformer {
    override def transformQuery[M](query: Query[M, _, _]): Query[M, _, _] = { query }
    override def transformModify[M](modify: ModifyQuery[M, _]): ModifyQuery[M, _] = { modify }
    override def transformFindAndModify[M, R](modify: FindAndModifyQuery[M, R]): FindAndModifyQuery[M, R] = { modify }
  }

  object NoopQueryTransformer extends DefaultQueryTransformer

  var transformer: QueryTransformer = NoopQueryTransformer

  trait QueryConfig {
    def defaultWriteConcern: WriteConcern
    def cursorBatchSize: Option[Option[Int]]
    def maxTimeMSOpt(configName: String): Option[Long]
  }

  class DefaultQueryConfig extends QueryConfig {
    override def defaultWriteConcern: WriteConcern = WriteConcern.ACKNOWLEDGED
    /**
     * Batch size to set on the underlying DBCursor.
     * None = take value from the query if specified
     * Some(None) = never set batch size on the cursor
     * Some(Some(n)) = always set batch size to n
     */
    override def cursorBatchSize: Option[Option[Int]] = None
    override def maxTimeMSOpt(configName: String): Option[Long] = None
  }

  object DefaultQueryConfig extends DefaultQueryConfig

  var config: QueryConfig = DefaultQueryConfig

  def makeJavaList[T](sl: Iterable[T]): java.util.List[T] = {
    val list = new java.util.ArrayList[T]()
    for (id <- sl) list.add(id)
    list
  }

  def validatedList[T](vs: Iterable[T]): java.util.List[T] = {
    validator.validateList(vs)
    makeJavaList(vs)
  }

  def list[T](vs: Iterable[T]): java.util.List[T] = {
    makeJavaList(vs)
  }

  def list(vs: Double*): java.util.List[Double] = list(vs)

  def radius(d: Degrees) = {
    validator.validateRadius(d).value
  }

  def makeJavaMap[K, V](m: Map[K, V]): java.util.Map[K, V] = {
    val map = new java.util.HashMap[K, V]
    for ((k, v) <- m) map.put(k, v)
    map
  }

  def inListClause[V](fieldName: String, vs: Iterable[V]) = {
    if (vs.isEmpty)
      new EmptyQueryClause[java.util.List[V]](fieldName)
    else
      new InQueryClause(fieldName, QueryHelpers.validatedList(vs.toSet))
  }

  def allListClause[V](fieldName: String, vs: Iterable[V]) = {
    if (vs.isEmpty)
      new EmptyQueryClause[java.util.List[V]](fieldName)
    else
      new AllQueryClause(fieldName, QueryHelpers.validatedList(vs.toSet))
  }

  def orConditionFromQueries(subqueries: List[Query[_, _, _]]) = {
    MongoHelpers.OrCondition(subqueries.flatMap(subquery => {
      subquery match {
        case q: Query[_, _, _] if q.condition.isEmpty => None
        case q: Query[_, _, _] => Some(q.condition)
        case _ => None
      }
    }))
  }
}
