package me.sgrouples.rogue.cc

import org.mongodb.scala._
import io.fsq.field.Field
import io.fsq.rogue.MongoHelpers.{MongoModify, MongoSelect}
import io.fsq.rogue._
import org.bson.BsonDocument
import org.mongodb.scala.result.{DeleteResult, InsertManyResult, InsertOneResult, UpdateResult}
import org.reactivestreams.Publisher

import scala.concurrent.Future
import scala.reflect.ClassTag

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
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], dba: MongoDatabase): Future[Long] = {
    adapter.count(query, readPreference)
  }

  def exists[M <: MB, State](
    query: Query[M, _, State],
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], dba: MongoDatabase): Future[Boolean] = {
    adapter.exists(query, readPreference)
  }

  def countDistinct[M <: MB, V :ClassTag, State](
    query: Query[M, _, State],
    readPreference: Option[ReadPreference] = None)(field: M => Field[V, M])(implicit ev: ShardingOk[M, State], dba: MongoDatabase): Future[Long] = {
    adapter.countDistinct(query, field(query.meta).name, readPreference)
  }

  def distinct[M <: MB, V: ClassTag, State](
    query: Query[M, _, State],
    readPreference: Option[ReadPreference] = None)(field: M => Field[V, M])(implicit ev: ShardingOk[M, State], dba: MongoDatabase): Future[Seq[V]] = {
    adapter.distinct(query, field(query.meta).name, readPreference)
  }

  def fetch[M <: MB, R, State](
    query: Query[M, R, State],
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], dba: MongoDatabase): Future[Seq[R]] = {
    val s = readSerializer[M, R](query.meta, query.select)

    adapter.find(query, s, readPreference)
  }

  def fetchPublisher[M <: MB, R, State](
    query: Query[M, R, State],
    batchSize: Int,
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], dba: MongoDatabase, ctR: ClassTag[R]): Publisher[R] = {
    val s = readSerializer[M, R](query.meta, query.select)
    adapter.findPublisher(query, s, batchSize, readPreference)
  }

  def fetchOne[M <: MB, R, State, S2](
    query: Query[M, R, State],
    readPreference: Option[ReadPreference] = None)(implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2], dba: MongoDatabase): Future[Option[R]] = {
    val q = query.limit(1)
    val s = readSerializer[M, R](q.meta, q.select)
    adapter.fineOne(q, s, readPreference)
  }

  def foreach[M <: MB, R, State](
    query: Query[M, R, State],
    readPreference: Option[ReadPreference] = None)(f: R => Unit)(implicit ev: ShardingOk[M, State], dba: MongoDatabase): Future[Unit] = {
    val s = readSerializer[M, R](query.meta, query.select)
    val docBlock: BsonDocument => Unit = doc => f(s.fromDocument(doc))
    //applies docBlock to each Document = conversion + f(R)
    adapter.foreach(query, docBlock, readPreference)
  }

  def bulkDelete_!![M <: MB, State](
    query: Query[M, _, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit
    ev1: Required[State, Unselected with Unlimited with Unskipped],
    ev2: ShardingOk[M, State], dba: MongoDatabase): Future[DeleteResult] = {
    adapter.delete(query, writeConcern)
  }

  def updateOne[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], dba: MongoDatabase): Future[UpdateResult] = {
    adapter.modify(query, upsert = false, multi = false, writeConcern = writeConcern)
  }

  def updateOneRet[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], dba: MongoDatabase): Future[UpdateResult] = {
    adapter.modifyRet(query, upsert = false, multi = false, writeConcern = writeConcern)
  }

  def upsertOne[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], dba: MongoDatabase): Future[UpdateResult] = {
    adapter.modify(query, upsert = true, multi = false, writeConcern = writeConcern)
  }

  def upsertOneRet[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], dba: MongoDatabase): Future[UpdateResult] = {
    adapter.modifyRet(query, upsert = true, multi = false, writeConcern = writeConcern)
  }

  def updateMulti[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit dba: MongoDatabase): Future[UpdateResult] = {
    adapter.modify(query, upsert = false, multi = true, writeConcern = writeConcern)
  }

  def updateMultiRet[M <: MB, State](
    query: ModifyQuery[M, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit dba: MongoDatabase): Future[UpdateResult] = {
    adapter.modifyRet(query, upsert = false, multi = true, writeConcern = writeConcern)
  }

  //WARNING - it might not behave like original - since I don't know how to handle selection
  def findAndUpdateOne[M <: MB, R](
    query: FindAndModifyQuery[M, R],
    returnNew: Boolean = false,
    writeConcern: WriteConcern = defaultWriteConcern)(implicit dba: MongoDatabase): Future[Option[R]] = {
    val s = readSerializer[M, R](query.query.meta, query.query.select)
    adapter.findAndModify(query, returnNew, upsert = false, remove = false, writeConcern)(s.fromDocumentOpt _)
  }

  def findAndUpsertOne[M <: MB, R](
    query: FindAndModifyQuery[M, R],
    returnNew: Boolean = false,
    writeConcern: WriteConcern = defaultWriteConcern)(implicit dba: MongoDatabase): Future[Option[R]] = {
    val s = readSerializer[M, R](query.query.meta, query.query.select)
    val ss = { dbo: BsonDocument =>
      val x = s.fromDocumentOpt(dbo)
      //println(s"Find and upsert one from ${dbo}, got ${x}")
      x
    }
    adapter.findAndModify(query, returnNew, upsert = true, remove = false, writeConcern)(s.fromDocumentOpt _)
  }

  def findAndDeleteOne[M <: MB, R, State](
    query: Query[M, R, State],
    writeConcern: WriteConcern = defaultWriteConcern)(implicit ev: RequireShardKey[M, State], dba: MongoDatabase): Future[Option[R]] = {
    val s = readSerializer[M, R](query.meta, query.select)
    val mod = FindAndModifyQuery(query, MongoModify(Nil))
    adapter.findAndModify(mod, returnNew = false, upsert = false, remove = true, writeConcern)(s.fromDocumentOpt _)
  }

  def insertOne[M <: MB, R](query: Query[M, R, _], r: R)(implicit dba: MongoDatabase): Future[InsertOneResult] = {
    val doc = writeSerializer[M, R](query.meta).toDocument(r)
    adapter.insertOne(query, doc, defaultWriteConcern)
  }

  def insertMany[M <: MB, R](query: Query[M, R, _], r: Seq[R])(implicit dba: MongoDatabase): Future[InsertManyResult] = {
    val s = writeSerializer[M, R](query.meta)
    val docs = r.map { d => s.toDocument(d) }
    adapter.insertMany(query, docs, defaultWriteConcern)
  }

  def replaceOne[M <: MB, R](query: Query[M, R, _], r: R, upsert: Boolean)(implicit dba: MongoDatabase): Future[UpdateResult] = {
    val s = writeSerializer[M, R](query.meta)
    adapter.replaceOne(query, s.toDocument(r), upsert, defaultWriteConcern)
  }

  /*def batch[M <: MB, R, T, State](
    query: Query[M, R, State],
    f: Seq[R] => Future[Seq[T]],
    batchSize: Int = 100,
    readPreference: Option[ReadPreference] = None)(implicit ev: ShardingOk[M, State], dba: MongoDatabase, ec: ExecutionContext): Future[Seq[T]] = {
    val s = readSerializer[M, R](query.meta, query.select)
    adapter.batch(query, s, f, batchSize, readPreference)
  }*/
}