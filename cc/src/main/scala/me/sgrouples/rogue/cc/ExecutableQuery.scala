package me.sgrouples.rogue.cc

// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
// Copyright 2016 Sgrouples Inc. All Rights Reserved.

import com.mongodb.{ ReadPreference, WriteConcern }
import io.fsq.field.Field
import io.fsq.rogue.{ AddLimit, FindAndModifyQuery, Iter, ModifyQuery, Query, RequireShardKey, Required, ShardingOk, Unlimited, Unselected, Unskipped, _ }
import io.fsq.rogue.MongoHelpers.MongoSelect
import com.mongodb.client.MongoDatabase
import com.mongodb.async.client.{ MongoDatabase => MongoAsyncDatabase }

import scala.concurrent.{ ExecutionContext, Future }
import scala.reflect.ClassTag

case class ExecutableQuery[MB, M <: MB, R, State](
  query: Query[M, R, State],
  ex: BsonExecutors[MB])(implicit ev: ShardingOk[M, State]) {

  /**
   * Gets the size of the query result. This should only be called on queries that do not
   * have limits or skips.
   */
  def count()(implicit db: MongoDatabase): Long =
    ex.sync.count(query)

  /**
   * Returns the number of distinct values returned by a query. The query must not have
   * limit or skip clauses.
   */
  def countDistinct[V](field: M => Field[V, _], readPreference: Option[ReadPreference] = None)(implicit ct: ClassTag[V], db: MongoDatabase): Long =
    ex.sync.countDistinct(query, readPreference, ct)(field.asInstanceOf[M => Field[V, M]])

  def countDistinctAsync[V](field: M => Field[V, _])(implicit ct: ClassTag[V], db: MongoAsyncDatabase): Future[Long] =
    ex.async.countDistinct(query, ct)(field.asInstanceOf[M => Field[V, M]])

  /**
   * Returns a list of distinct values returned by a query. The query must not have
   * limit or skip clauses.
   */

  def distinct[V](field: M => Field[V, _], readPreference: Option[ReadPreference] = None)(implicit ct: ClassTag[V], db: MongoDatabase): Seq[V] =
    ex.sync.distinct(query, readPreference, ct)(field.asInstanceOf[M => Field[V, M]]).toSeq

  /**
   * Returns a list of distinct values returned by a query. The query must not have
   * limit or skip clauses.
   */
  def distinctAsync[V](field: M => Field[V, _])(implicit ct: ClassTag[V], db: MongoAsyncDatabase): Future[Seq[V]] = {
    ex.async.distinct(query, ct)(field.asInstanceOf[M => Field[V, M]])
  }

  /**
   * Checks if there are any records that match this query.
   */
  def exists()(implicit ev: State <:< Unlimited with Unskipped, db: MongoDatabase): Boolean = {
    val q = query.copy(select = Some(MongoSelect[M, Null](IndexedSeq.empty, _ => null, true, query.select.flatMap(_.scoreName))))
    ex.sync.fetch(q.limit(1)).size > 0
  }

  /**
   * Executes a function on each record value returned by a query.
   * @param f a function to be invoked on each fetched record.
   * @return nothing.
   */

  def foreach(f: R => Unit)(implicit db: MongoDatabase): Unit =
    ex.sync.foreach(query)(f)

  /**
   * Execute the query, returning all of the records that match the query.
   * @return a list containing the records that match the query
   */

  def fetch()(implicit db: MongoDatabase): List[R] =
    ex.sync.fetchList(query)

  /**
   * Execute a query, returning no more than a specified number of result records. The
   * query must not have a limit clause.
   * @param limit the maximum number of records to return.
   */

  def fetch[S2](limit: Int)(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2], db: MongoDatabase): List[R] =
    ex.sync.fetchList(query.limit(limit))

  /**
   * fetch a batch of results, and execute a function on each element of the list.
   * @param f the function to invoke on the records that match the query.
   * @return a list containing the results of invoking the function on each record.
   */
  def fetchBatch[T](batchSize: Int)(f: Seq[R] => Seq[T])(implicit db: MongoDatabase): Seq[T] =
    ex.sync.fetchBatch(query, batchSize)(f)

  /**
   * Fetches the first record that matches the query. The query must not contain a "limited" clause.
   * @return an option record containing either the first result that matches the
   *         query, or None if there are no records that match.
   */
  def get[S2]()(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2], db: MongoDatabase): Option[R] =
    ex.sync.fetchOne(query)

  /**
   * Delete all of the records that match the query. The query must not contain any "skip",
   * "limit", or "select" clauses. Sends the delete operation to mongo, and returns - does
   * <em>not</em> wait for the delete to be finished.
   */
  def bulkDelete_!!!()(implicit ev1: Required[State, Unselected with Unlimited with Unskipped], db: MongoDatabase): Unit =
    ex.sync.bulkDelete_!!(query)

  /**
   * Delete all of the records that match the query. The query must not contain any "skip",
   * "limit", or "select" clauses. Sends the delete operation to mongo, and waits for the
   * delete operation to complete before returning to the caller.
   */
  def bulkDelete_!!(concern: WriteConcern)(implicit ev1: Required[State, Unselected with Unlimited with Unskipped], db: MongoDatabase): Unit =
    ex.sync.bulkDelete_!!(query, concern)

  /**
   * Finds the first record that matches the query (if any), fetches it, and then deletes it.
   * A copy of the deleted record is returned to the caller.
   */
  def findAndDeleteOne()(implicit ev: RequireShardKey[M, State], db: MongoDatabase): Option[R] =
    ex.sync.findAndDeleteOne(query)

  def iterate[S](state: S)(handler: (S, Iter.Event[R]) => Iter.Command[S])(implicit db: MongoDatabase): S =
    ex.sync.iterate(query, state)(handler)

  def iterateBatch[S](batchSize: Int, state: S)(handler: (S, Iter.Event[Seq[R]]) => Iter.Command[S])(implicit db: MongoDatabase): S =
    ex.sync.iterateBatch(query, batchSize, state)(handler)

  // async ops
  def countAsync()(implicit dba: MongoAsyncDatabase): Future[Long] = ex.async.count(query)

  def foreachAsync(f: R => Unit)(implicit dba: MongoAsyncDatabase): Future[Unit] = ex.async.foreach(query)(f)

  def fetchAsync()(implicit dba: MongoAsyncDatabase): Future[Seq[R]] = ex.async.fetch(query)

  def fetchAsync[S2](limit: Int)(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2], dba: MongoAsyncDatabase): Future[Seq[R]] = ex.async.fetch(query.limit(limit))

  def getAsync[S2]()(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2], dba: MongoAsyncDatabase): Future[Option[R]] = ex.async.fetchOne(query)

  /*def paginateAsync(countPerPage: Int)
                   (implicit ev1: Required[State, Unlimited with Unskipped],
                    ev2: ShardingOk[M, State]) = {
    new PaginatedQuery(ev1(query),  dba, countPerPage)
  }
*/
  def existsAsync(implicit ev: State <:< Unlimited with Unskipped, dba: MongoAsyncDatabase): Future[Boolean] = {
    val q = query.copy(select = Some(MongoSelect[M, Null](IndexedSeq.empty, _ => null, true, query.select.flatMap(_.scoreName))))
    ex.async.exists(q.limit(1))
  }

  def bulkDeleteAsync_!!!()(implicit ev1: Required[State, Unselected with Unlimited with Unskipped], dba: MongoAsyncDatabase): Future[Unit] = ex.async.bulkDelete_!!(query)

  def bulkDeleteAsync_!!(concern: WriteConcern)(implicit ev1: Required[State, Unselected with Unlimited with Unskipped], dba: MongoAsyncDatabase): Future[Unit] = ex.async.bulkDelete_!!(query, concern)

  def findAndDeleteOneAsync()(implicit ev: RequireShardKey[M, State], dba: MongoAsyncDatabase): Future[Option[R]] = ex.async.findAndDeleteOne(query)

  def batchAsync[T](f: Seq[R] => Future[Seq[T]], batchSize: Int = 100, readPreference: Option[ReadPreference] = None)(implicit dba: MongoAsyncDatabase, ec: ExecutionContext): Future[Seq[T]] =
    ex.async.batch(query, f, batchSize, readPreference)

}

case class ExecutableModifyQuery[MB, M <: MB, State](
  query: ModifyQuery[M, State],
  ex: BsonExecutors[MB]) {
  def updateMulti()(implicit db: MongoDatabase): Unit =
    ex.sync.updateMulti(query)

  def updateOne()(implicit ev: RequireShardKey[M, State], db: MongoDatabase): Unit =
    ex.sync.updateOne(query)

  def upsertOne()(implicit ev: RequireShardKey[M, State], db: MongoDatabase): Unit =
    ex.sync.upsertOne(query)

  def updateMulti(writeConcern: WriteConcern)(implicit db: MongoDatabase): Unit =
    ex.sync.updateMulti(query, writeConcern)

  def updateOne(writeConcern: WriteConcern)(implicit ev: RequireShardKey[M, State], db: MongoDatabase): Unit =
    ex.sync.updateOne(query, writeConcern)

  def upsertOne(writeConcern: WriteConcern)(implicit ev: RequireShardKey[M, State], db: MongoDatabase): Unit =
    ex.sync.upsertOne(query, writeConcern)

  //async ops
  def updateMultiAsync()(implicit dba: MongoAsyncDatabase): Future[Unit] =
    ex.async.updateMulti(query)

  def updateOneAsync()(implicit ev: RequireShardKey[M, State], dba: MongoAsyncDatabase): Future[Unit] =
    ex.async.updateOne(query)

  def upsertOneAsync()(implicit ev: RequireShardKey[M, State], dba: MongoAsyncDatabase): Future[Unit] =
    ex.async.upsertOne(query)

  def updateMultiAsync(writeConcern: WriteConcern)(implicit dba: MongoAsyncDatabase): Future[Unit] =
    ex.async.updateMulti(query, writeConcern)

  def updateOneAsync(writeConcern: WriteConcern)(implicit ev: RequireShardKey[M, State], dba: MongoAsyncDatabase): Future[Unit] =
    ex.async.updateOne(query, writeConcern)

  def upsertOneAsync(writeConcern: WriteConcern)(implicit ev: RequireShardKey[M, State], dba: MongoAsyncDatabase): Future[Unit] =
    ex.async.upsertOne(query, writeConcern)

}

case class ExecutableFindAndModifyQuery[MB, M <: MB, R](
  query: FindAndModifyQuery[M, R],
  ex: BsonExecutors[MB]) {
  def updateOne(returnNew: Boolean = false)(implicit db: MongoDatabase): Option[R] =
    ex.sync.findAndUpdateOne(query, returnNew)

  def upsertOne(returnNew: Boolean = false)(implicit db: MongoDatabase): Option[R] =
    ex.sync.findAndUpsertOne(query, returnNew)

  def updateOneAsync(returnNew: Boolean = false)(implicit dba: MongoAsyncDatabase): Future[Option[R]] =
    ex.async.findAndUpdateOne(query, returnNew)

  def upsertOneAsync(returnNew: Boolean = false)(implicit dba: MongoAsyncDatabase): Future[Option[R]] =
    ex.async.findAndUpsertOne(query, returnNew)

}

case class InsertableQuery[MB, M <: MB, R, State](
  query: Query[M, R, State],
  ex: BsonExecutors[MB]) {

  def insertOneAsync(t: R)(implicit dba: MongoAsyncDatabase): Future[Unit] = {
    ex.async.insertOne(query, t)
  }
  def insertManyAsync(ts: Seq[R])(implicit dba: MongoAsyncDatabase): Future[Unit] = {
    if (ts.nonEmpty)
      ex.async.insertMany(query, ts)
    else
      Future.unit
  }

  /**
   * *
   * replaces document with new one, matching by `_id` field. if such document does not exists and `upsert` == `true` new document is inserted
   * @param t: a document
   * @param upsert boolean parameter
   * @param dba Database connection
   */
  def replaceOneAsync(t: R, upsert: Boolean = true)(implicit dba: MongoAsyncDatabase): Future[Unit] = ex.async.replaceOne(query, t, upsert)

  def insertOne(t: R)(implicit db: MongoDatabase): Unit = ex.sync.insertOne(query, t)

  def insertMany(ts: Seq[R])(implicit db: MongoDatabase): Unit = {
    if (ts.nonEmpty)
      ex.sync.insertMany(query, ts)
    else
      Future.unit
  }

  /**
   * *
   * replaces document with new one, matching by `_id` field. if such document does not exists and `upsert` == `true` new document is inserted
   * @param t: a document
   * @param upsert boolean parameter
   * @param db Database connection
   */
  def replaceOne(t: R, upsert: Boolean = true)(implicit db: MongoDatabase): Unit = ex.sync.replaceOne(query, t, upsert)

}
