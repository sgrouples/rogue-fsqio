package me.sgrouples.rogue.cc

// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.

import java.util.function.Consumer

import com.mongodb._
import com.mongodb.async.client.{ MongoDatabase => MongoAsyncDatabase }
import com.mongodb.client.MongoDatabase
import io.fsq.field.Field
import io.fsq.rogue.MongoHelpers.{ MongoModify, MongoSelect }
import io.fsq.rogue._
import io.fsq.spindle.types.MongoDisallowed
import org.bson.{ BsonDocument, BsonValue }

import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.collection.mutable.{ Builder, ListBuffer }

trait RogueBsonRead[R] {
  def fromDocument(dbo: BsonDocument): R
  def fromDocumentOpt(dbo: BsonDocument): Option[R]
}

trait RogueBsonWrite[R] {
  def toDocument(record: R): BsonDocument
}

trait RogueBsonSerializer[From, To] extends RogueBsonRead[To] with RogueBsonWrite[From]

trait ReadWriteSerializers[MB] {

  protected def readSerializer[M <: MB, R](
    meta: M,
    select: Option[MongoSelect[M, R]]): RogueBsonRead[R]

  protected def writeSerializer[M <: MB, R](meta: M): RogueBsonWrite[R]

  def defaultWriteConcern: WriteConcern

}

trait AsyncBsonQueryExecutor[MB] extends ReadWriteSerializers[MB] with Rogue {
  def adapter: MongoAsyncBsonJavaDriverAdapter[MB]

  def count[M <: MB, State](
    query: Query[M, _, State],
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], dba: MongoAsyncDatabase): Future[Long] = {
    adapter.count(query, readPreference)
  }

  def exists[M <: MB, State](
    query: Query[M, _, State],
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], dba: MongoAsyncDatabase): Future[Boolean] = {
    adapter.exists(query, readPreference)
  }

  def countDistinct[M <: MB, V, State](
    query: Query[M, _, State],
    ct: ClassTag[V],
    readPreference: Option[ReadPreference] = None)(field: M => Field[V, M])(implicit ev: ShardingOk[M, State], dba: MongoAsyncDatabase): Future[Long] = {
    adapter.countDistinct(query, field(query.meta).name, ct, readPreference)
  }

  def distinct[M <: MB, V, State](
    query: Query[M, _, State],
    ct: ClassTag[V],
    readPreference: Option[ReadPreference] = None)(field: M => Field[V, M])(implicit ev: ShardingOk[M, State], dba: MongoAsyncDatabase): Future[Seq[V]] = {
    adapter.distinct(query, field(query.meta).name, ct, readPreference)
  }

  def fetch[M <: MB, R, State](
    query: Query[M, R, State],
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], dba: MongoAsyncDatabase): Future[Seq[R]] = {
    val s = readSerializer[M, R](query.meta, query.select)
    adapter.find(query, s)
  }

  def fetchOne[M <: MB, R, State, S2](
    query: Query[M, R, State],
    readPreference: Option[ReadPreference] = None)(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2], dba: MongoAsyncDatabase): Future[Option[R]] = {
    val q = query.limit(1)
    val s = readSerializer[M, R](q.meta, q.select)
    adapter.fineOne(q, s)
  }

  def foreach[M <: MB, R, State](
    query: Query[M, R, State],
    readPreference: Option[ReadPreference] = None)(f: R => Unit)(implicit ev: ShardingOk[M, State], dba: MongoAsyncDatabase): Future[Unit] = {
    val s = readSerializer[M, R](query.meta, query.select)
    val docBlock: BsonDocument => Unit = doc => f(s.fromDocument(doc))
    //applies docBlock to each Document = conversion + f(R)
    adapter.foreach(query, docBlock)
  }

  def bulkDelete_!![M <: MB, State](
    query: Query[M, _, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit
    ev1: Required[State, Unselected with Unlimited with Unskipped],
    ev2: ShardingOk[M, State], dba: MongoAsyncDatabase): Future[Unit] = {
    adapter.delete(query, writeConcern)
  }

  def updateOne[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], dba: MongoAsyncDatabase): Future[Unit] = {
    adapter.modify(query, upsert = false, multi = false, writeConcern = writeConcern)
  }

  def upsertOne[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], dba: MongoAsyncDatabase): Future[Unit] = {
    adapter.modify(query, upsert = true, multi = false, writeConcern = writeConcern)
  }

  def updateMulti[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit dba: MongoAsyncDatabase): Future[Unit] = {
    adapter.modify(query, upsert = false, multi = true, writeConcern = writeConcern)
  }

  //WARNING - it might not behave like original - since I don't know how to handle selection
  def findAndUpdateOne[M <: MB, R](
    query: FindAndModifyQuery[M, R],
    returnNew: Boolean = false,
    writeConcern: WriteConcern = defaultWriteConcern)(implicit dba: MongoAsyncDatabase): Future[Option[R]] = {
    val s = readSerializer[M, R](query.query.meta, query.query.select)
    adapter.findAndModify(query, returnNew, upsert = false, remove = false)(s.fromDocumentOpt _)
  }

  def findAndUpsertOne[M <: MB, R](
    query: FindAndModifyQuery[M, R],
    returnNew: Boolean = false,
    writeConcern: WriteConcern = defaultWriteConcern)(implicit dba: MongoAsyncDatabase): Future[Option[R]] = {
    val s = readSerializer[M, R](query.query.meta, query.query.select)
    val ss = { dbo: BsonDocument =>
      val x = s.fromDocumentOpt(dbo)
      //println(s"Find and upsert one from ${dbo}, got ${x}")
      x
    }
    adapter.findAndModify(query, returnNew, upsert = true, remove = false)(s.fromDocumentOpt _)
  }

  def findAndDeleteOne[M <: MB, R, State](
    query: Query[M, R, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], dba: MongoAsyncDatabase): Future[Option[R]] = {
    val s = readSerializer[M, R](query.meta, query.select)
    val mod = FindAndModifyQuery(query, MongoModify(Nil))
    adapter.findAndModify(mod, returnNew = false, upsert = false, remove = true)(s.fromDocumentOpt _)
  }

  def insertOne[M <: MB, R](query: Query[M, R, _], r: R)(implicit dba: MongoAsyncDatabase): Future[Unit] = {
    val doc = writeSerializer[M, R](query.meta).toDocument(r)
    adapter.insertOne(query, doc, defaultWriteConcern)
  }

  def insertMany[M <: MB, R](query: Query[M, R, _], r: Seq[R])(implicit dba: MongoAsyncDatabase): Future[Unit] = {
    val s = writeSerializer[M, R](query.meta)
    val docs = r.map { d => s.toDocument(d) }
    adapter.insertMany(query, docs, defaultWriteConcern)
  }

  def replaceOne[M <: MB, R](query: Query[M, R, _], r: R, upsert: Boolean)(implicit dba: MongoAsyncDatabase): Future[Unit] = {
    val s = writeSerializer[M, R](query.meta)
    adapter.replaceOne(query, s.toDocument(r), upsert, defaultWriteConcern)
  }
}

trait BsonQueryExecutor[MB] extends ReadWriteSerializers[MB] with Rogue {
  def adapter: MongoBsonJavaDriverAdapter[MB]

  def count[M <: MB, State](
    query: Query[M, _, State],
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed, db: MongoDatabase): Long = {
    adapter.count(query, readPreference)
  }

  def countDistinct[M <: MB, V, State](
    query: Query[M, _, State],
    readPreference: Option[ReadPreference] = None,
    ct: ClassTag[V])(field: M => Field[V, M])(implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed, db: MongoDatabase): Long = {
    adapter.countDistinct(query, field(query.meta).name, ct, readPreference)
  }

  def distinct[M <: MB, V, State](
    query: Query[M, _, State],
    readPreference: Option[ReadPreference] = None,
    ct: ClassTag[V])(field: M => Field[V, M])(implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed, db: MongoDatabase): Iterable[V] = {
    //TODO - probably wrong
    adapter.distinct(query, field(query.meta).name, ct, readPreference)
  }

  def fetchList[M <: MB, R, State](
    query: Query[M, R, State],
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed, db: MongoDatabase): List[R] = {
    val s = readSerializer[M, R](query.meta, query.select)
    adapter.find(query, s, readPreference).toList
  }

  def fetch[M <: MB, R, State](
    query: Query[M, R, State],
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed, db: MongoDatabase): Seq[R] = {
    val s = readSerializer[M, R](query.meta, query.select)
    adapter.find(query, s, readPreference)
  }

  def fetchOne[M <: MB, R, State, S2](
    query: Query[M, R, State],
    readPreference: Option[ReadPreference] = None,
    masterFallback: Boolean = false)(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2], ev3: M !<:< MongoDisallowed, db: MongoDatabase): Option[R] = {

    val initialResult = fetch(query.limit(1), readPreference).headOption

    val needToRetry = {
      masterFallback && initialResult.isEmpty && !readPreference.exists(_ == ReadPreference.primary)
    }

    if (needToRetry) {
      fetch(query.limit(1), Some(ReadPreference.primary)).headOption
    } else {
      initialResult
    }
  }

  def foreach[M <: MB, R, State](
    query: Query[M, R, State],
    readPreference: Option[ReadPreference] = None)(f: R => Unit)(implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed, db: MongoDatabase): Unit = {
    val s = readSerializer[M, R](query.meta, query.select)
    val action = new Consumer[BsonDocument]() {
      override def accept(t: BsonDocument): Unit = {
        s.fromDocument(t)
      }
    }
    adapter.findIterable(query, None, readPreference).forEach(action)
  }

  private def drainBufferList[A, B](
    from: ListBuffer[A],
    to: ListBuffer[B],
    f: List[A] => List[B],
    size: Int): Unit = {
    // ListBuffer#length is O(1) vs ListBuffer#size is O(N) (true in 2.9.x, fixed in 2.10.x)
    if (from.length >= size) {
      to ++= f(from.toList)
      from.clear
    }
  }

  def fetchBatchList[M <: MB, R, T, State](
    query: Query[M, R, State],
    batchSize: Int,
    readPreference: Option[ReadPreference] = None)(
    f: List[R] => List[T])(
    implicit
    ev: ShardingOk[M, State],
    ev2: M !<:< MongoDisallowed, db: MongoDatabase): List[T] = {
    val s = readSerializer[M, R](query.meta, query.select)
    val rv = new ListBuffer[T]
    val buf = new ListBuffer[R]
    val action = new Consumer[BsonDocument] {
      override def accept(t: BsonDocument): Unit = {
        buf += s.fromDocument(t)
        drainBufferList(buf, rv, f, batchSize)
      }
    }
    adapter.findIterable(query, Some(batchSize), readPreference).forEach(action)
    drainBufferList(buf, rv, f, 1)
    rv.toList
  }

  private def drainBufferSeq[A, B](
    from: ListBuffer[A],
    to: Builder[B, Vector[B]],
    f: Seq[A] => Seq[B],
    size: Int): Unit = {
    // ListBuffer#length is O(1) vs ListBuffer#size is O(N) (true in 2.9.x, fixed in 2.10.x)
    if (from.length >= size) {
      to ++= f(from.toList)
      from.clear
    }
  }

  def fetchBatch[M <: MB, R, T, State](
    query: Query[M, R, State],
    batchSize: Int,
    readPreference: Option[ReadPreference] = None)(
    f: Seq[R] => Seq[T])(
    implicit
    ev: ShardingOk[M, State],
    ev2: M !<:< MongoDisallowed, db: MongoDatabase): Seq[T] = {
    val s = readSerializer[M, R](query.meta, query.select)
    val rv = Vector.newBuilder[T]
    val buf = new ListBuffer[R]
    val action = new Consumer[BsonDocument] {
      override def accept(t: BsonDocument): Unit = {
        buf += s.fromDocument(t)
        drainBufferSeq(buf, rv, f, batchSize)
      }
    }
    adapter.findIterable(query, Some(batchSize), readPreference).forEach(action)
    drainBufferSeq(buf, rv, f, 1)
    rv.result
  }

  def bulkDelete_!![M <: MB, State](
    query: Query[M, _, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit
    ev1: Required[State, Unselected with Unlimited with Unskipped],
    ev2: ShardingOk[M, State], ev3: M !<:< MongoDisallowed, db: MongoDatabase): Unit = {
    adapter.delete(query, writeConcern)
  }

  def updateOne[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], db: MongoDatabase): Unit = {
    adapter.modify(query, upsert = false, multi = false, writeConcern = writeConcern)
  }

  def upsertOne[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], db: MongoDatabase): Unit = {
    try {
      adapter.modify(query, upsert = true, multi = false, writeConcern = writeConcern)
    } catch {
      case r: RogueException if r.getCause() != null && r.getCause().isInstanceOf[DuplicateKeyException] => {
        /* NOTE: have to retry upserts that fail with duplicate key,
           * see https://jira.mongodb.org/browse/SERVER-14322
           */
        adapter.modify(query, upsert = true, multi = false, writeConcern = writeConcern)
      }
    }
  }

  def updateMulti[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit db: MongoDatabase): Unit = {
    adapter.modify(query, upsert = false, multi = true, writeConcern = writeConcern)
  }

  def findAndUpdateOne[M <: MB, R](
    query: FindAndModifyQuery[M, R],
    returnNew: Boolean = false,
    writeConcern: WriteConcern = defaultWriteConcern)(implicit db: MongoDatabase): Option[R] = {
    val s = readSerializer[M, R](query.query.meta, query.query.select)
    adapter.findAndModify(query, returnNew, upsert = false, remove = false)(s.fromDocument _)
  }

  def findAndUpsertOne[M <: MB, R](
    query: FindAndModifyQuery[M, R],
    returnNew: Boolean = false,
    writeConcern: WriteConcern = defaultWriteConcern)(implicit db: MongoDatabase): Option[R] = {
    try {
      val s = readSerializer[M, R](query.query.meta, query.query.select)
      adapter.findAndModify(query, returnNew, upsert = true, remove = false)(s.fromDocument _)
    } catch {
      case r: RogueException if r.getCause() != null && r.getCause().isInstanceOf[DuplicateKeyException] => {
        /* NOTE: have to retry upserts that fail with duplicate key,
           * see https://jira.mongodb.org/browse/SERVER-14322
           */
        val s = readSerializer[M, R](query.query.meta, query.query.select)
        adapter.findAndModify(query, returnNew, upsert = true, remove = false)(s.fromDocument _)
      }
    }
  }

  def findAndDeleteOne[M <: MB, R, State](
    query: Query[M, R, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], db: MongoDatabase): Option[R] = {
    val s = readSerializer[M, R](query.meta, query.select)
    val mod = FindAndModifyQuery(query, MongoModify(Nil))
    adapter.findAndModify(mod, returnNew = false, upsert = false, remove = true)(s.fromDocument _)
  }

  def iterate[S, M <: MB, R, State](
    query: Query[M, R, State],
    state: S,
    readPreference: Option[ReadPreference] = None)(handler: (S, Iter.Event[R]) => Iter.Command[S])(implicit ev: ShardingOk[M, State], db: MongoDatabase): S = {
    val s = readSerializer[M, R](query.meta, query.select)
    adapter.iterate(query, state, s.fromDocument _, readPreference)(handler)
  }

  def iterateBatch[S, M <: MB, R, State](
    query: Query[M, R, State],
    batchSize: Int,
    state: S,
    readPreference: Option[ReadPreference] = None)(handler: (S, Iter.Event[Seq[R]]) => Iter.Command[S])(implicit ev: ShardingOk[M, State], db: MongoDatabase): S = {
    val s = readSerializer[M, R](query.meta, query.select)
    adapter.iterateBatch(query, batchSize, state, s.fromDocument _, readPreference)(handler)
    handler(state, Iter.EOF).state
  }

  def insertOne[M <: MB, R](query: Query[M, R, _], r: R)(implicit db: MongoDatabase): Unit = {
    val doc = writeSerializer[M, R](query.meta).toDocument(r)
    adapter.insertOne(query, doc, defaultWriteConcern)
  }

  def insertMany[M <: MB, R](query: Query[M, R, _], r: Seq[R])(implicit db: MongoDatabase): Unit = {
    val s = writeSerializer[M, R](query.meta)
    val docs = r.map { d => s.toDocument(d) }
    adapter.insertMany(query, docs, defaultWriteConcern)
  }

  def replaceOne[M <: MB, R](query: Query[M, R, _], r: R, upsert: Boolean)(implicit db: MongoDatabase): Unit = {
    val s = writeSerializer[M, R](query.meta)
    adapter.replaceOne(query, s.toDocument(r), upsert, defaultWriteConcern)
  }

}