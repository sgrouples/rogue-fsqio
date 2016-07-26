package me.sgrouples.rogue.cc

// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.
// Copyright 2016 Sgrouples Inc. All Rights Reserved.

import com.mongodb.{ ReadPreference, WriteConcern }
import io.fsq.field.Field
import io.fsq.rogue.{ AddLimit, FindAndModifyQuery, Iter, ModifyQuery, Query, RequireShardKey, Required, ShardingOk, Unlimited, Unselected, Unskipped, _ }
import io.fsq.rogue.MongoHelpers.MongoSelect

import scala.concurrent.Future
import scala.reflect.ClassTag

case class ExecutableQuery[MB, M <: MB, R, State](
    query: Query[M, R, State],
    ex: BsonExecutors[MB]
)(implicit ev: ShardingOk[M, State]) {

  /**
   * Gets the size of the query result. This should only be called on queries that do not
   * have limits or skips.
   */
  def count(): Long =
    ex.sync.count(query)

  /**
   * Returns the number of distinct values returned by a query. The query must not have
   * limit or skip clauses.
   */
  def countDistinct[V](field: M => Field[V, _], readPreference: Option[ReadPreference] = None)(implicit ct: ClassTag[V]): Long =
    ex.sync.countDistinct(query, readPreference, ct)(field.asInstanceOf[M => Field[V, M]])

  def countDistinctAsync[V](field: M => Field[V, _])(implicit ct: ClassTag[V]): Future[Long] =
    ex.async.countDistinct(query, ct)(field.asInstanceOf[M => Field[V, M]])

  /**
   * Returns a list of distinct values returned by a query. The query must not have
   * limit or skip clauses.
   */

  def distinct[V](field: M => Field[V, _], readPreference: Option[ReadPreference] = None)(implicit ct: ClassTag[V]): Seq[V] =
    ex.sync.distinct(query, readPreference, ct)(field.asInstanceOf[M => Field[V, M]]).toSeq

  /**
   * Returns a list of distinct values returned by a query. The query must not have
   * limit or skip clauses.
   */
  def distinctAsync[V](field: M => Field[V, _])(implicit ct: ClassTag[V]): Future[Seq[V]] = {
    ex.async.distinct(query, ct)(field.asInstanceOf[M => Field[V, M]])
  }

  /**
   * Checks if there are any records that match this query.
   */
  def exists()(implicit ev: State <:< Unlimited with Unskipped): Boolean = {
    val q = query.copy(select = Some(MongoSelect[M, Null](IndexedSeq.empty, _ => null)))
    ex.sync.fetch(q.limit(1)).size > 0
  }

  /**
   * Executes a function on each record value returned by a query.
   * @param f a function to be invoked on each fetched record.
   * @return nothing.
   */

  def foreach(f: R => Unit): Unit =
    ex.sync.foreach(query)(f)

  /**
   * Execute the query, returning all of the records that match the query.
   * @return a list containing the records that match the query
   */

  def fetch(): List[R] =
    ex.sync.fetchList(query)

  /**
   * Execute a query, returning no more than a specified number of result records. The
   * query must not have a limit clause.
   * @param limit the maximum number of records to return.
   */

  def fetch[S2](limit: Int)(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2]): List[R] =
    ex.sync.fetchList(query.limit(limit))

  /**
   * fetch a batch of results, and execute a function on each element of the list.
   * @param f the function to invoke on the records that match the query.
   * @return a list containing the results of invoking the function on each record.
   */
  def fetchBatch[T](batchSize: Int)(f: Seq[R] => Seq[T]): Seq[T] =
    ex.sync.fetchBatch(query, batchSize)(f)

  /**
   * Fetches the first record that matches the query. The query must not contain a "limited" clause.
   * @return an option record containing either the first result that matches the
   *         query, or None if there are no records that match.
   */
  def get[S2]()(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2]): Option[R] =
    ex.sync.fetchOne(query)

  /**
   * Delete all of the records that match the query. The query must not contain any "skip",
   * "limit", or "select" clauses. Sends the delete operation to mongo, and returns - does
   * <em>not</em> wait for the delete to be finished.
   */
  def bulkDelete_!!!()(implicit ev1: Required[State, Unselected with Unlimited with Unskipped]): Unit =
    ex.sync.bulkDelete_!!(query)

  /**
   * Delete all of the records that match the query. The query must not contain any "skip",
   * "limit", or "select" clauses. Sends the delete operation to mongo, and waits for the
   * delete operation to complete before returning to the caller.
   */
  def bulkDelete_!!(concern: WriteConcern)(implicit ev1: Required[State, Unselected with Unlimited with Unskipped]): Unit =
    ex.sync.bulkDelete_!!(query, concern)

  /**
   * Finds the first record that matches the query (if any), fetches it, and then deletes it.
   * A copy of the deleted record is returned to the caller.
   */
  def findAndDeleteOne()(implicit ev: RequireShardKey[M, State]): Option[R] =
    ex.sync.findAndDeleteOne(query)

  def iterate[S](state: S)(handler: (S, Iter.Event[R]) => Iter.Command[S]): S =
    ex.sync.iterate(query, state)(handler)

  def iterateBatch[S](batchSize: Int, state: S)(handler: (S, Iter.Event[Seq[R]]) => Iter.Command[S]): S =
    ex.sync.iterateBatch(query, batchSize, state)(handler)

  // async ops
  def countAsync(): Future[Long] = ex.async.count(query)

  def foreachAsync(f: R => Unit): Future[Unit] = ex.async.foreach(query)(f)

  def fetchAsync(): Future[Seq[R]] = ex.async.fetch(query)

  def fetchAsync[S2](limit: Int)(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2]): Future[Seq[R]] = ex.async.fetch(query.limit(limit))

  def getAsync[S2]()(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2]): Future[Option[R]] = ex.async.fetchOne(query)

  /*def paginateAsync(countPerPage: Int)
                   (implicit ev1: Required[State, Unlimited with Unskipped],
                    ev2: ShardingOk[M, State]) = {
    new PaginatedQuery(ev1(query),  dba, countPerPage)
  }
*/
  def existsAsync(implicit ev: State <:< Unlimited with Unskipped): Future[Boolean] = {
    val q = query.copy(select = Some(MongoSelect[M, Null](IndexedSeq.empty, _ => null)))
    ex.async.exists(q.limit(1))
  }

  def bulkDeleteAsync_!!!()(implicit ev1: Required[State, Unselected with Unlimited with Unskipped]): Future[Unit] = ex.async.bulkDelete_!!(query)

  def bulkDeleteAsync_!!(concern: WriteConcern)(implicit ev1: Required[State, Unselected with Unlimited with Unskipped]): Future[Unit] = ex.async.bulkDelete_!!(query, concern)

  def findAndDeleteOneAsync()(implicit ev: RequireShardKey[M, State]): Future[Option[R]] = ex.async.findAndDeleteOne(query)

}

case class ExecutableModifyQuery[MB, M <: MB, State](
    query: ModifyQuery[M, State],
    ex: BsonExecutors[MB]
) {
  def updateMulti(): Unit =
    ex.sync.updateMulti(query)

  def updateOne()(implicit ev: RequireShardKey[M, State]): Unit =
    ex.sync.updateOne(query)

  def upsertOne()(implicit ev: RequireShardKey[M, State]): Unit =
    ex.sync.upsertOne(query)

  def updateMulti(writeConcern: WriteConcern): Unit =
    ex.sync.updateMulti(query, writeConcern)

  def updateOne(writeConcern: WriteConcern)(implicit ev: RequireShardKey[M, State]): Unit =
    ex.sync.updateOne(query, writeConcern)

  def upsertOne(writeConcern: WriteConcern)(implicit ev: RequireShardKey[M, State]): Unit =
    ex.sync.upsertOne(query, writeConcern)

  //async ops
  def updateMultiAsync(): Future[Unit] =
    ex.async.updateMulti(query)

  def updateOneAsync()(implicit ev: RequireShardKey[M, State]): Future[Unit] =
    ex.async.updateOne(query)

  def upsertOneAsync()(implicit ev: RequireShardKey[M, State]): Future[Unit] =
    ex.async.upsertOne(query)

  def updateMultiAsync(writeConcern: WriteConcern): Future[Unit] =
    ex.async.updateMulti(query, writeConcern)

  def updateOneAsync(writeConcern: WriteConcern)(implicit ev: RequireShardKey[M, State]): Future[Unit] =
    ex.async.updateOne(query, writeConcern)

  def upsertOneAsync(writeConcern: WriteConcern)(implicit ev: RequireShardKey[M, State]): Future[Unit] =
    ex.async.upsertOne(query, writeConcern)

}

case class ExecutableFindAndModifyQuery[MB, M <: MB, R](
    query: FindAndModifyQuery[M, R],
    ex: BsonExecutors[MB]
) {
  def updateOne(returnNew: Boolean = false): Option[R] =
    ex.sync.findAndUpdateOne(query, returnNew)

  def upsertOne(returnNew: Boolean = false): Option[R] =
    ex.sync.findAndUpsertOne(query, returnNew)

  def updateOneAsync(returnNew: Boolean = false): Future[Option[R]] =
    ex.async.findAndUpdateOne(query, returnNew)

  def upsertOneAsync(returnNew: Boolean = false): Future[Option[R]] =
    ex.async.findAndUpsertOne(query, returnNew)

}

case class InsertableQuery[MB, M <: MB, R, State](
    query: Query[M, R, State],
    ex: BsonExecutors[MB]
) {

  def insertOneAsync(t: R): Future[Unit] = {
    ex.async.insertOne(query, t)
  }
  def insertManyAsync(ts: Seq[R]): Future[Unit] = {
    ex.async.insertMany(query, ts)
  }

  def insertOne(t: R): Unit = ex.sync.insertOne(query, t)

  def insertMany(ts: Seq[R]): Unit = ex.sync.insertMany(query, ts)
}
