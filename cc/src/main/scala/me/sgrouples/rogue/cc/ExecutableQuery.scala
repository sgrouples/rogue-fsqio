package me.sgrouples.rogue.cc

// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
// Copyright 2016 Sgrouples Inc. All Rights Reserved.

import com.mongodb.client.result.InsertManyResult
import io.fsq.field.Field
import io.fsq.rogue.MongoHelpers.MongoSelect
import io.fsq.rogue.{
  AddLimit,
  FindAndModifyQuery,
  ModifyQuery,
  Query,
  RequireShardKey,
  Required,
  ShardingOk,
  Unlimited,
  Unselected,
  Unskipped,
  _
}
import me.sgrouples.rogue.ListField
import org.mongodb.scala._
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.result.{
  DeleteResult,
  InsertManyResult,
  InsertOneResult,
  UpdateResult
}
import org.reactivestreams.Publisher

import scala.concurrent.{blocking, ExecutionContext, Future}
import scala.reflect.ClassTag

case class ExecutableQuery[MB, M <: MB, R, State](
    query: Query[M, R, State],
    ex: BsonExecutors[MB]
)(implicit ev: ShardingOk[M, State]) {
  import Waiter._

  /** Gets the size of the query result. This should only be called on queries
    * that do not have limits or skips.
    */
  def count()(implicit db: MongoDatabase): Long = waitForFuture(countAsync())

  def estimatedDocumentCount()(implicit
      ev1: Required[State, InitialState],
      db: MongoDatabase
  ): Long =
    waitForFuture(estimatedDocumentCountAsync())

  /** Returns the number of distinct values returned by a query. The query must
    * not have limit or skip clauses.
    */
  def countDistinct[V](
      field: M => Field[V, _],
      readPreference: Option[ReadPreference] = None
  )(implicit ct: ClassTag[V], db: MongoDatabase): Long = {
    waitForFuture(countDistinctAsync(field, readPreference))
  }

  def countDistinctAsync[V](
      field: M => Field[V, _],
      readPreference: Option[ReadPreference] = None
  )(implicit ct: ClassTag[V], db: MongoDatabase): Future[Long] =
    ex.async.countDistinct(query, readPreference)(
      field.asInstanceOf[M => Field[V, M]]
    )

  /** Returns a list of distinct values returned by a query. The query must not
    * have limit or skip clauses.
    */

  def distinct[V: ClassTag](
      field: M => Field[V, _],
      readPreference: Option[ReadPreference] = None
  )(implicit db: MongoDatabase): Seq[V] =
    waitForFuture(distinctAsync(field, readPreference))

  /** Returns a list of distinct values returned by a query. The query must not
    * have limit or skip clauses.
    */
  def distinctAsync[V: ClassTag](
      field: M => Field[V, _],
      readPreference: Option[ReadPreference] = None
  )(implicit db: MongoDatabase): Future[Seq[V]] = {
    ex.async.distinct(query, readPreference)(
      field.asInstanceOf[M => Field[V, M]]
    )
  }

  /** Like above but must be called with explicit types. Usefull when the field
    * type is an Array/List/Seq. See here:
    * https://www.mongodb.com/docs/manual/reference/command/distinct/#return-distinct-values-for-an-array-field
    */
  def distinctAsyncT[V: ClassTag, GenField[_, _] <: Field[_, _]](
      field: M => GenField[V, _],
      readPreference: Option[ReadPreference] = None
  )(implicit db: MongoDatabase): Future[Seq[V]] = {
    ex.async.distinct(query, readPreference)(
      field.asInstanceOf[M => Field[V, M]]
    )
  }

  /** Checks if there are any records that match this query.
    */
  def exists()(implicit
      ev: State <:< Unlimited with Unskipped,
      db: MongoDatabase
  ): Boolean = {
    waitForFuture(existsAsync())
  }

  /** Executes a function on each record value returned by a query.
    * @param f
    *   a function to be invoked on each fetched record.
    * @return
    *   nothing.
    */

  def foreach(f: R => Unit)(implicit db: MongoDatabase): Unit =
    waitForFuture(foreachAsync(f))

  /** Execute the query, returning all of the records that match the query.
    * @return
    *   a list containing the records that match the query
    */

  def fetch()(implicit db: MongoDatabase): Seq[R] = {
    waitForFuture(fetchAsync())
  }

  /** fetch a batch of results, and execute a function on each element of the
    * list.
    * @param f
    *   the function to invoke on the records that match the query.
    * @return
    *   a list containing the results of invoking the function on each record.
    */
  def fetchBatch[T](batchSize: Int)(f: Seq[R] => Seq[T])(implicit
      db: MongoDatabase,
      ct: ClassTag[R],
      ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  ): Seq[T] = {
    def adaptedF(in: Seq[R]): Future[Seq[T]] = {
      Future(blocking(f(in)))
    }
    waitForFuture(batchAsync(adaptedF, batchSize))
  }

  /** Fetches the first record that matches the query. The query must not
    * contain a "limited" clause.
    * @return
    *   an option record containing either the first result that matches the
    *   query, or None if there are no records that match.
    */
  def get[S2]()(implicit
      ev1: AddLimit[State, S2],
      ev2: ShardingOk[M, S2],
      db: MongoDatabase
  ): Option[R] = {
    waitForFuture(getAsync())
  }

  /** Delete all of the records that match the query. The query must not contain
    * any "skip", "limit", or "select" clauses. Sends the delete operation to
    * mongo, and returns - does <em>not</em> wait for the delete to be finished.
    */
  def bulkDelete_!!!()(implicit
      ev1: Required[State, Unselected with Unlimited with Unskipped],
      db: MongoDatabase
  ): Unit =
    waitForFuture(bulkDeleteAsync_!!!())

  /** Delete all of the records that match the query. The query must not contain
    * any "skip", "limit", or "select" clauses. Sends the delete operation to
    * mongo, and waits for the delete operation to complete before returning to
    * the caller.
    */
  def bulkDelete_!!(concern: WriteConcern)(implicit
      ev1: Required[State, Unselected with Unlimited with Unskipped],
      db: MongoDatabase
  ): Unit = {
    waitForFuture(bulkDeleteAsync_!!(concern))
  }

  /** Finds the first record that matches the query (if any), fetches it, and
    * then deletes it. A copy of the deleted record is returned to the caller.
    */
  def findAndDeleteOne()(implicit
      ev: RequireShardKey[M, State],
      db: MongoDatabase
  ): Option[R] =
    waitForFuture(findAndDeleteOneAsync())

  // async ops
  def countAsync()(implicit dba: MongoDatabase): Future[Long] =
    ex.async.count(query)

  def estimatedDocumentCountAsync()(implicit
      ev1: Required[State, InitialState],
      dba: MongoDatabase
  ): Future[Long] =
    ex.async.estimatedDocumentCount(query)

  def foreachAsync(f: R => Unit)(implicit dba: MongoDatabase): Future[Unit] =
    ex.async.foreach(query)(f)

  def fetchAsync()(implicit dba: MongoDatabase): Future[Seq[R]] =
    ex.async.fetch(query)

  def fetchPublisher(
      batchSize: Int = 20
  )(implicit dba: MongoDatabase, ctR: ClassTag[R]): Publisher[R] =
    ex.async.fetchPublisher(query, batchSize)

  def fetchAsync[S2](limit: Int)(implicit
      ev1: AddLimit[State, S2],
      ev2: ShardingOk[M, S2],
      dba: MongoDatabase
  ): Future[Seq[R]] = ex.async.fetch(query.limit(limit))

  def getAsync[S2]()(implicit
      ev1: AddLimit[State, S2],
      ev2: ShardingOk[M, S2],
      dba: MongoDatabase
  ): Future[Option[R]] = ex.async.fetchOne(query)

  def existsAsync()(implicit
      ev: State <:< Unlimited with Unskipped,
      dba: MongoDatabase
  ): Future[Boolean] = {
    val q = query.copy(select =
      Some(
        MongoSelect[M, Null](
          IndexedSeq.empty,
          _ => null,
          true,
          query.select.flatMap(_.scoreName)
        )
      )
    )
    ex.async.exists(q.limit(1))
  }

  def bulkDeleteAsync_!!!()(implicit
      ev1: Required[State, Unselected with Unlimited with Unskipped],
      dba: MongoDatabase
  ): Future[DeleteResult] = ex.async.bulkDelete_!!(query)

  def bulkDeleteAsync_!!(concern: WriteConcern)(implicit
      ev1: Required[State, Unselected with Unlimited with Unskipped],
      dba: MongoDatabase
  ): Future[DeleteResult] = ex.async.bulkDelete_!!(query, concern)

  def findAndDeleteOneAsync()(implicit
      ev: RequireShardKey[M, State],
      dba: MongoDatabase
  ): Future[Option[R]] = ex.async.findAndDeleteOne(query)

  def batchAsync[T](
      f: Seq[R] => Future[Seq[T]],
      batchSize: Int = 100,
      readPreference: Option[ReadPreference] = None
  )(implicit
      dba: MongoDatabase,
      ec: ExecutionContext,
      ct: ClassTag[R]
  ): Future[Seq[T]] =
    ex.async.batch(query, f, batchSize, readPreference)

}

case class ExecutableModifyQuery[MB, M <: MB, State](
    query: ModifyQuery[M, State],
    ex: BsonExecutors[MB]
) {
  import Waiter._

  def updateMulti()(implicit db: MongoDatabase): Unit =
    waitForFuture(updateMultiAsync())

  def updateOne()(implicit
      ev: RequireShardKey[M, State],
      db: MongoDatabase
  ): Unit =
    waitForFuture(updateOneAsync())

  def upsertOne()(implicit
      ev: RequireShardKey[M, State],
      db: MongoDatabase
  ): Unit =
    waitForFuture(upsertOneAsync())

  def updateMulti(writeConcern: WriteConcern)(implicit
      db: MongoDatabase
  ): Unit =
    waitForFuture(updateMultiAsync(writeConcern))

  def updateOne(
      writeConcern: WriteConcern
  )(implicit ev: RequireShardKey[M, State], db: MongoDatabase): Unit =
    waitForFuture(updateOneAsync(writeConcern))

  def upsertOne(
      writeConcern: WriteConcern
  )(implicit ev: RequireShardKey[M, State], db: MongoDatabase): Unit =
    waitForFuture(upsertOneAsync(writeConcern))

  //async ops
  def updateMultiAsync()(implicit dba: MongoDatabase): Future[UpdateResult] =
    ex.async.updateMulti(query)

  def updateOneAsync()(implicit
      ev: RequireShardKey[M, State],
      dba: MongoDatabase
  ): Future[UpdateResult] =
    ex.async.updateOne(query)

  def upsertOneAsync()(implicit
      ev: RequireShardKey[M, State],
      dba: MongoDatabase
  ): Future[UpdateResult] =
    ex.async.upsertOne(query)

  def updateMultiAsync(writeConcern: WriteConcern)(implicit
      dba: MongoDatabase
  ): Future[UpdateResult] =
    ex.async.updateMulti(query, writeConcern)

  def updateMultiAsyncRet(writeConcern: WriteConcern)(implicit
      dba: MongoDatabase
  ): Future[UpdateResult] =
    ex.async.updateMultiRet(query, writeConcern)

  def updateOneAsync(writeConcern: WriteConcern)(implicit
      ev: RequireShardKey[M, State],
      dba: MongoDatabase
  ): Future[UpdateResult] =
    ex.async.updateOne(query, writeConcern)

  def updateOneAsyncRet(writeConcern: WriteConcern)(implicit
      ev: RequireShardKey[M, State],
      dba: MongoDatabase
  ): Future[UpdateResult] =
    ex.async.updateOneRet(query, writeConcern)

  def upsertOneAsync(writeConcern: WriteConcern)(implicit
      ev: RequireShardKey[M, State],
      dba: MongoDatabase
  ): Future[UpdateResult] =
    ex.async.upsertOne(query, writeConcern)

  def upsertOneAsyncRet(writeConcern: WriteConcern)(implicit
      ev: RequireShardKey[M, State],
      dba: MongoDatabase
  ): Future[UpdateResult] =
    ex.async.upsertOneRet(query, writeConcern)

}

case class ExecutableFindAndModifyQuery[MB, M <: MB, R](
    query: FindAndModifyQuery[M, R],
    ex: BsonExecutors[MB]
) {
  import Waiter._

  def updateOne(returnNew: Boolean = false)(implicit
      db: MongoDatabase
  ): Option[R] =
    waitForFuture(updateOneAsync(returnNew))

  def upsertOne(returnNew: Boolean = false)(implicit
      db: MongoDatabase
  ): Option[R] =
    waitForFuture(upsertOneAsync(returnNew))

  def updateOneAsync(returnNew: Boolean = false)(implicit
      dba: MongoDatabase
  ): Future[Option[R]] =
    ex.async.findAndUpdateOne(query, returnNew)

  def upsertOneAsync(returnNew: Boolean = false)(implicit
      dba: MongoDatabase
  ): Future[Option[R]] =
    ex.async.findAndUpsertOne(query, returnNew)

}

case class InsertableQuery[MB, M <: MB, R, State](
    query: Query[M, R, State],
    ex: BsonExecutors[MB]
) {
  import Waiter._

  def insertOneAsync(
      t: R,
      writeConcern: WriteConcern
  )(implicit dba: MongoDatabase): Future[InsertOneResult] = {
    ex.async.insertOne(query, t, writeConcern)
  }

  def insertOneAsync(
      t: R
  )(implicit dba: MongoDatabase): Future[InsertOneResult] = {
    ex.async.insertOne(query, t)
  }

  def insertManyAsync(
      ts: Seq[R]
  )(implicit dba: MongoDatabase): Future[InsertManyResult] = {
    if (ts.nonEmpty)
      ex.async.insertMany(query, ts)
    else
      Future.successful(InsertManyResult.unacknowledged())
  }

  /** * replaces document with new one, matching by `_id` field. if such
    * document does not exists and `upsert` == `true` new document is inserted
    * @param t:
    *   a document
    * @param upsert
    *   boolean parameter
    * @param dba
    *   Database connection
    */
  def replaceOneAsync(t: R, upsert: Boolean = true)(implicit
      dba: MongoDatabase
  ): Future[UpdateResult] =
    ex.async.replaceOne(query, t, upsert)

  def insertOne(t: R)(implicit db: MongoDatabase): Unit =
    waitForFuture(insertOneAsync(t))

  def insertMany(ts: Seq[R])(implicit db: MongoDatabase): Unit =
    if (ts.nonEmpty) {
      waitForFuture(insertManyAsync(ts))
    }

  /** * replaces document with new one, matching by `_id` field. if such
    * document does not exists and `upsert` == `true` new document is inserted
    * @param t:
    *   a document
    * @param upsert
    *   boolean parameter
    * @param db
    *   Database connection
    */
  def replaceOne(t: R, upsert: Boolean = true)(implicit
      db: MongoDatabase
  ): Unit =
    waitForFuture(replaceOneAsync(t, upsert))

}

case class AggregateQuery[MB, M <: MB,  State](collectionName:String, ex: BsonExecutors[_]) {
  def aggregateSeq[R : ClassTag](pipeline: Seq[Bson], mapper:Document => R, readPreference: ReadPreference = ReadPreference.primaryPreferred(),
                                 allowDiskUse: Boolean = false)(implicit db:MongoDatabase):Future[Seq[R]] = {
    getCollection(db, collectionName, readPreference)
      .aggregate(pipeline)
      .allowDiskUse(allowDiskUse)
      .map(mapper)
      .collect[R]().toFuture()
  }
  def aggregateFoldLeft[R : ClassTag](pipeline: Seq[Bson], zero:R, acc: (R, Document) => R, readPreference: ReadPreference = ReadPreference.primaryPreferred(),
                                      allowDiskUse: Boolean = false)(implicit db: MongoDatabase): Future[R] = {
    getCollection(db, collectionName, readPreference)
      .aggregate(pipeline)
      .allowDiskUse(allowDiskUse)
      .foldLeft(zero)(acc)
      .toFuture()
  }

  private[this] def getCollection(
      db: MongoDatabase,
      collectionName: String,
      readPreference: ReadPreference
  ) =
    db.getCollection(collectionName).withReadPreference(readPreference)
}
