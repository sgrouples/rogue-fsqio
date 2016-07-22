package me.sgrouples.rogue.cc

// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.

import com.mongodb._
import io.fsq.field.Field
import io.fsq.rogue.MongoHelpers.{MongoModify, MongoSelect}
import io.fsq.rogue._
import io.fsq.spindle.types.MongoDisallowed
import org.bson.BsonDocument

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.collection.mutable.{Builder, ListBuffer}

trait RogueBsonRead[R] {
  def fromDocument(dbo: BsonDocument): R
  def fromDocumentOpt(dbo: BsonDocument): Option[R]
}

trait RogueBsonWrite[R] {
  def toDocument(record: R): BsonDocument
}

trait RogueBsonSerializer[From, To] extends RogueBsonRead[To] with RogueBsonWrite[From]

/*trait QueryExecutor[MB, RB] extends Rogue {
  def adapter: MongoJavaDriverAdapter[MB, RB]
  def optimizer: QueryOptimizer

  def defaultWriteConcern: WriteConcern

  protected def readSerializer[M <: MB, R](
                                            meta: M,
                                            select: Option[MongoSelect[M, R]]
                                          ): RogueReadSerializer[R]

  protected def writeSerializer(record: RB): RogueWriteSerializer[RB]

  def count[M <: MB, State](query: Query[M, _, State],
                            readPreference: Option[ReadPreference] = None)
                           (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): Long = {
    if (optimizer.isEmptyQuery(query)) {
      0L
    } else {
      adapter.count(query, readPreference)
    }
  }

  def countDistinct[M <: MB, V, State](query: Query[M, _, State],
                                       readPreference: Option[ReadPreference] = None)
                                      (field: M => Field[V, M])
                                      (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): Long = {
    if (optimizer.isEmptyQuery(query)) {
      0L
    } else {
      adapter.countDistinct(query, field(query.meta).name, readPreference)
    }
  }

  def distinct[M <: MB, V, State](query: Query[M, _, State],
                                  readPreference: Option[ReadPreference] = None)
                                 (field: M => Field[V, M])
                                 (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): Seq[V] = {
    if (optimizer.isEmptyQuery(query)) {
      Nil
    } else {
      val rv = Vector.newBuilder[V]
      adapter.distinct[M, V](query, field(query.meta).name, readPreference)(s => rv += s)
      rv.result
    }
  }

  def fetchList[M <: MB, R, State](query: Query[M, R, State],
                                   readPreference: Option[ReadPreference] = None)
                                  (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): List[R] = {
    if (optimizer.isEmptyQuery(query)) {
      Nil
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val rv = new ListBuffer[R]
      adapter.query(query, None, readPreference)(dbo => rv += s.fromDocument(dbo))
      rv.toList
    }
  }

  def fetch[M <: MB, R, State](query: Query[M, R, State],
                               readPreference: Option[ReadPreference] = None)
                              (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): Seq[R] = {
    if (optimizer.isEmptyQuery(query)) {
      Nil
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val rv = Vector.newBuilder[R]
      adapter.query(query, None, readPreference)(dbo => rv += s.fromDocument(dbo))
      rv.result
    }
  }

  def fetchOne[M <: MB, R, State, S2](query: Query[M, R, State],
                                      readPreference: Option[ReadPreference] = None,
                                      masterFallback: Boolean = false)
                                     (implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2], ev3: M !<:< MongoDisallowed): Option[R] = {

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

  def foreach[M <: MB, R, State](query: Query[M, R, State],
                                 readPreference: Option[ReadPreference] = None)
                                (f: R => Unit)
                                (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): Unit = {
    if (optimizer.isEmptyQuery(query)) {
      ()
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      adapter.query(query, None, readPreference)(dbo => f(s.fromDocument(dbo)))
    }
  }

  private def drainBufferList[A, B](
                                     from: ListBuffer[A],
                                     to: ListBuffer[B],
                                     f: List[A] => List[B],
                                     size: Int
                                   ): Unit = {
    // ListBuffer#length is O(1) vs ListBuffer#size is O(N) (true in 2.9.x, fixed in 2.10.x)
    if (from.length >= size) {
      to ++= f(from.toList)
      from.clear
    }
  }

  def fetchBatchList[M <: MB, R, T, State](
                                            query: Query[M, R, State],
                                            batchSize: Int,
                                            readPreference: Option[ReadPreference] = None
                                          )(
                                            f: List[R] => List[T]
                                          )(
                                            implicit ev: ShardingOk[M, State],
                                            ev2: M !<:< MongoDisallowed
                                          ): List[T] = {
    if (optimizer.isEmptyQuery(query)) {
      Nil
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val rv = new ListBuffer[T]
      val buf = new ListBuffer[R]

      adapter.query(query, Some(batchSize), readPreference) { dbo =>
        buf += s.fromDocument(dbo)
        drainBufferList(buf, rv, f, batchSize)
      }
      drainBufferList(buf, rv, f, 1)

      rv.toList
    }
  }

  private def drainBufferSeq[A, B](
                                    from: ListBuffer[A],
                                    to: Builder[B, Vector[B]],
                                    f: Seq[A] => Seq[B],
                                    size: Int
                                  ): Unit = {
    // ListBuffer#length is O(1) vs ListBuffer#size is O(N) (true in 2.9.x, fixed in 2.10.x)
    if (from.length >= size) {
      to ++= f(from.toList)
      from.clear
    }
  }

  def fetchBatch[M <: MB, R, T, State](
                                        query: Query[M, R, State],
                                        batchSize: Int,
                                        readPreference: Option[ReadPreference] = None
                                      )(
                                        f: Seq[R] => Seq[T]
                                      )(
                                        implicit ev: ShardingOk[M, State],
                                        ev2: M !<:< MongoDisallowed
                                      ): Seq[T] = {
    if (optimizer.isEmptyQuery(query)) {
      Nil
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val rv = Vector.newBuilder[T]
      val buf = new ListBuffer[R]

      adapter.query(query, Some(batchSize), readPreference) { dbo =>
        buf += s.fromDocument(dbo)
        drainBufferSeq(buf, rv, f, batchSize)
      }
      drainBufferSeq(buf, rv, f, 1)

      rv.result
    }
  }

  def bulkDelete_!![M <: MB, State](query: Query[M, _, State],
                                    writeConcern: WriteConcern = defaultWriteConcern)
                                   (implicit ev1: Required[State, Unselected with Unlimited with Unskipped],
                                    ev2: ShardingOk[M, State], ev3: M !<:< MongoDisallowed): Unit = {
    if (optimizer.isEmptyQuery(query)) {
      ()
    } else {
      adapter.delete(query, writeConcern)
    }
  }

  def updateOne[M <: MB, State](
                                 query: ModifyQuery[M, State],
                                 writeConcern: WriteConcern = defaultWriteConcern
                               )(implicit ev: RequireShardKey[M, State]): Unit = {
    if (optimizer.isEmptyQuery(query)) {
      ()
    } else {
      adapter.modify(query, upsert = false, multi = false, writeConcern = writeConcern)
    }
  }

  def upsertOne[M <: MB, State](
                                 query: ModifyQuery[M, State],
                                 writeConcern: WriteConcern = defaultWriteConcern
                               )(implicit ev: RequireShardKey[M, State]): Unit = {
    if (optimizer.isEmptyQuery(query)) {
      ()
    } else {
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
  }

  def updateMulti[M <: MB, State](
                                   query: ModifyQuery[M, State],
                                   writeConcern: WriteConcern = defaultWriteConcern
                                 ): Unit = {
    if (optimizer.isEmptyQuery(query)) {
      ()
    } else {
      adapter.modify(query, upsert = false, multi = true, writeConcern = writeConcern)
    }
  }

  def findAndUpdateOne[M <: MB, R](
                                    query: FindAndModifyQuery[M, R],
                                    returnNew: Boolean = false,
                                    writeConcern: WriteConcern = defaultWriteConcern
                                  ): Option[R] = {
    if (optimizer.isEmptyQuery(query)) {
      None
    } else {
      val s = readSerializer[M, R](query.query.meta, query.query.select)
      adapter.findAndModify(query, returnNew, upsert=false, remove=false)(s.fromDocument _)
    }
  }

  def findAndUpsertOne[M <: MB, R](
                                    query: FindAndModifyQuery[M, R],
                                    returnNew: Boolean = false,
                                    writeConcern: WriteConcern = defaultWriteConcern
                                  ): Option[R] = {
    if (optimizer.isEmptyQuery(query)) {
      None
    } else {
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
  }

  def findAndDeleteOne[M <: MB, R, State](
                                           query: Query[M, R, State],
                                           writeConcern: WriteConcern = defaultWriteConcern
                                         )(implicit ev: RequireShardKey[M, State]): Option[R] = {
    if (optimizer.isEmptyQuery(query)) {
      None
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val mod = FindAndModifyQuery(query, MongoModify(Nil))
      adapter.findAndModify(mod, returnNew=false, upsert=false, remove=true)(s.fromDocument _)
    }
  }

  def explain[M <: MB](query: Query[M, _, _]): String = {
    adapter.explain(query)
  }

  def iterate[S, M <: MB, R, State](query: Query[M, R, State],
                                    state: S,
                                    readPreference: Option[ReadPreference] = None)
                                   (handler: (S, Iter.Event[R]) => Iter.Command[S])
                                   (implicit ev: ShardingOk[M, State]): S = {
    if (optimizer.isEmptyQuery(query)) {
      handler(state, Iter.EOF).state
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      adapter.iterate(query, state, s.fromDocument _, readPreference)(handler)
    }
  }

  def iterateBatch[S, M <: MB, R, State](query: Query[M, R, State],
                                         batchSize: Int,
                                         state: S,
                                         readPreference: Option[ReadPreference] = None)
                                        (handler: (S, Iter.Event[Seq[R]]) => Iter.Command[S])
                                        (implicit ev: ShardingOk[M, State]): S = {
    if (optimizer.isEmptyQuery(query)) {
      handler(state, Iter.EOF).state
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      adapter.iterateBatch(query, batchSize, state, s.fromDocument _, readPreference)(handler)
    }
  }

  def save[RecordType <: RB](record: RecordType, writeConcern: WriteConcern = defaultWriteConcern): RecordType = {
    val s = writeSerializer(record)
    val dbo = s.toDocument(record)
    adapter.save(record, dbo, writeConcern)
    record
  }

  def insert[RecordType <: RB](record: RecordType, writeConcern: WriteConcern = defaultWriteConcern): RecordType = {
    val s = writeSerializer(record)
    val dbo = s.toDocument(record)
    adapter.insert(record, dbo, writeConcern)
    record
  }

  def insertAll[RecordType <: RB](records: Seq[RecordType], writeConcern: WriteConcern = defaultWriteConcern): Seq[RecordType] = {
    records.headOption.foreach(record => {
      val s = writeSerializer(record)
      val dbos = records.map(s.toDocument)
      adapter.insertAll(record, dbos, writeConcern)
    })
    records
  }

  def remove[RecordType <: RB](record: RecordType, writeConcern: WriteConcern = defaultWriteConcern): RecordType = {
    val s = writeSerializer(record)
    val dbo = s.toDocument(record)
    adapter.remove(record, dbo, writeConcern)
    record
  }
}
*/

trait AsyncBsonQueryExecutor[MB] extends Rogue {
  def adapter: MongoAsyncBsonJavaDriverAdapter[MB]

  def optimizer: QueryOptimizer

  def defaultWriteConcern: WriteConcern

  protected def readSerializer[M <: MB, R]( meta: M,
                                            select: Option[MongoSelect[M, R]]
                                          ): RogueBsonRead[R]

  protected def writeSerializer[M <: MB, R](meta: M): RogueBsonWrite[R]

  def count[M <: MB, State](query: Query[M, _, State],
                            readPreference: Option[ReadPreference] = None)
                           (implicit ev: ShardingOk[M, State]): Future[Long] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(0L)
    } else {
      adapter.count(query, readPreference)
    }
  }

  def exists[M <: MB, State](query: Query[M, _, State],
                             readPreference: Option[ReadPreference] = None)
                            (implicit ev: ShardingOk[M, State]): Future[Boolean] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(false)
    } else {
      adapter.exists(query, readPreference)
    }
  }

  def countDistinct[M <: MB, V, State](query: Query[M, _, State],
                                       ct: ClassTag[V],
                                       readPreference: Option[ReadPreference] = None)
                                      (field: M => Field[V, M])
                                      (implicit ev: ShardingOk[M, State]): Future[Long] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(0L)
    } else {
      adapter.countDistinct(query, field(query.meta).name, ct, readPreference)
    }
  }


  def distinct[M <: MB, V, State](query: Query[M, _, State],
                                  ct: ClassTag[V],
                                  readPreference: Option[ReadPreference] = None)
                                 (field: M => Field[V, M])
                                 (implicit ev: ShardingOk[M, State]): Future[Seq[V]] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(Nil)
    } else {
      adapter.distinct(query, field(query.meta).name, ct, readPreference)
    }
  }

  def fetch[M <: MB, R, State](query: Query[M, R, State],
                               readPreference: Option[ReadPreference] = None)
                              (implicit ev: ShardingOk[M, State]): Future[Seq[R]] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(Nil)
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      adapter.find(query, s)
    }
  }


  def fetchOne[M <: MB, R, State, S2](query: Query[M, R, State],
                                      readPreference: Option[ReadPreference] = None)
                                     (implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2]): Future[Option[R]] = {
    val q = query.limit(1)
    val s = readSerializer[M, R](q.meta, q.select)
    adapter.fineOne(q, s)
  }


  def foreach[M <: MB, R, State](query: Query[M, R, State],
                                 readPreference: Option[ReadPreference] = None)
                                (f: R => Unit)
                                (implicit ev: ShardingOk[M, State]): Future[Unit] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(())
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val docBlock: BsonDocument => Unit = doc => f(s.fromDocument(doc))
      //applies docBlock to each Document = conversion + f(R)
      adapter.foreach(query, docBlock)
    }
  }


  def bulkDelete_!![M <: MB, State](query: Query[M, _, State],
                                    writeConcern: WriteConcern = defaultWriteConcern)
                                   (implicit ev1: Required[State, Unselected with Unlimited with Unskipped],
                                    ev2: ShardingOk[M, State]): Future[Unit] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(())
    } else {
      adapter.delete(query, writeConcern)
    }
  }


  def updateOne[M <: MB, State](
                                 query: ModifyQuery[M, State],
                                 writeConcern: WriteConcern = defaultWriteConcern
                               )(implicit ev: RequireShardKey[M, State]): Future[Unit] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(())
    } else {
      adapter.modify(query, upsert = false, multi = false, writeConcern = writeConcern)
    }
  }

  def upsertOne[M <: MB, State](
                                 query: ModifyQuery[M, State],
                                 writeConcern: WriteConcern = defaultWriteConcern
                               )(implicit ev: RequireShardKey[M, State]): Future[Unit] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(())
    } else {
      adapter.modify(query, upsert = true, multi = false, writeConcern = writeConcern)
    }
  }


  def updateMulti[M <: MB, State](
                                   query: ModifyQuery[M, State],
                                   writeConcern: WriteConcern = defaultWriteConcern
                                 ): Future[Unit] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(())
    } else {
      adapter.modify(query, upsert = false, multi = true, writeConcern = writeConcern)
    }
  }


  //WARNING - it might not behave like original - since I don't know how to handle selection
  def findAndUpdateOne[M <: MB, R](
                                    query: FindAndModifyQuery[M, R],
                                    returnNew: Boolean = false,
                                    writeConcern: WriteConcern = defaultWriteConcern
                                  ): Future[Option[R]] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(None)
    } else {
      val s = readSerializer[M, R](query.query.meta, query.query.select)
      adapter.findAndModify(query, returnNew, upsert = false, remove = false)(s.fromDocumentOpt _)
    }
  }


  def findAndUpsertOne[M <: MB, R](
                                    query: FindAndModifyQuery[M, R],
                                    returnNew: Boolean = false,
                                    writeConcern: WriteConcern = defaultWriteConcern
                                  ): Future[Option[R]] = {
    if (optimizer.isEmptyQuery(query)) {
      println("EMpty query!!")
      Future.successful(None)
    } else {
      val s = readSerializer[M, R](query.query.meta, query.query.select)
      adapter.findAndModify(query, returnNew, upsert = true, remove = false)(s.fromDocumentOpt _)
    }
  }

  def findAndDeleteOne[M <: MB, R, State](
                                           query: Query[M, R, State],
                                           writeConcern: WriteConcern = defaultWriteConcern
                                         )(implicit ev: RequireShardKey[M, State]): Future[Option[R]] = {
    if (optimizer.isEmptyQuery(query)) {
      Future.successful(None)
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val mod = FindAndModifyQuery(query, MongoModify(Nil))
      adapter.findAndModify(mod, returnNew = false, upsert = false, remove = true)(s.fromDocumentOpt _)
    }
  }

  def insertOne[M <: MB, R](query: Query[M,R,_], r:R):Future[Unit] = {
    val doc = writeSerializer[M, R](query.meta).toDocument(r)
    adapter.insertOne(query, doc, defaultWriteConcern)
  }

  def insertMany[M <: MB, R](query: Query[M,R,_], r:Seq[R]):Future[Unit] = {
    val s = writeSerializer[M, R](query.meta)
    val docs = r.map{ d => s.toDocument(r.asInstanceOf[R])}
    adapter.insertMany(query, docs, defaultWriteConcern)
  }

}


trait BsonQueryExecutor[MB <: CcMetaLike[_]] extends Rogue {
  def adapter: MongoBsonJavaDriverAdapter[MB]
  def optimizer: QueryOptimizer

  def defaultWriteConcern: WriteConcern

  protected def readSerializer[M <: MB, R]( meta: M,
                                            select: Option[MongoSelect[M, R]]
                                          ): RogueBsonRead[R]

  protected def writeSerializer[M <: MB](meta: M): RogueBsonWrite[meta.R]

  def count[M <: MB, State](query: Query[M, _, State],
                            readPreference: Option[ReadPreference] = None)
                           (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): Long = {
    if (optimizer.isEmptyQuery(query)) {
      0L
    } else {
      adapter.count(query, readPreference)
    }
  }

  def countDistinct[M <: MB, V, State](query: Query[M, _, State],
                                       readPreference: Option[ReadPreference] = None)
                                      (field: M => Field[V, M])
                                      (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): Long = {
    if (optimizer.isEmptyQuery(query)) {
      0L
    } else {
      adapter.countDistinct(query, field(query.meta).name, readPreference)
    }
  }

  def distinct[M <: MB, V, State](query: Query[M, _, State],
                                  readPreference: Option[ReadPreference] = None)
                                 (field: M => Field[V, M])
                                 (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): Seq[V] = {
    if (optimizer.isEmptyQuery(query)) {
      Nil
    } else {
      val rv = Vector.newBuilder[V]
    //  adapter.distinct[M, V](query, field(query.meta).name, readPreference)(s => rv += s)
      rv.result
    }
  }

  def fetchList[M <: MB, R, State](query: Query[M, R, State],
                                   readPreference: Option[ReadPreference] = None)
                                  (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): List[R] = {
    if (optimizer.isEmptyQuery(query)) {
      Nil
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val rv = new ListBuffer[R]
      adapter.query(query, None, readPreference)(dbo => rv += s.fromDocument(dbo))
      rv.toList
    }
  }

  def fetch[M <: MB, R, State](query: Query[M, R, State],
                               readPreference: Option[ReadPreference] = None)
                              (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): Seq[R] = {
    if (optimizer.isEmptyQuery(query)) {
      Nil
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val rv = Vector.newBuilder[R]
      adapter.query(query, None, readPreference)(dbo => rv += s.fromDocument(dbo))
      rv.result
    }
  }

  def fetchOne[M <: MB, R, State, S2](query: Query[M, R, State],
                                      readPreference: Option[ReadPreference] = None,
                                      masterFallback: Boolean = false)
                                     (implicit ev1: AddLimit[State, S2], ev2: ShardingOk[M, S2], ev3: M !<:< MongoDisallowed): Option[R] = {

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

  def foreach[M <: MB, R, State](query: Query[M, R, State],
                                 readPreference: Option[ReadPreference] = None)
                                (f: R => Unit)
                                (implicit ev: ShardingOk[M, State], ev2: M !<:< MongoDisallowed): Unit = {
    if (optimizer.isEmptyQuery(query)) {
      ()
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      adapter.query(query, None, readPreference)(dbo => f(s.fromDocument(dbo)))
    }
  }

  private def drainBufferList[A, B](
                                     from: ListBuffer[A],
                                     to: ListBuffer[B],
                                     f: List[A] => List[B],
                                     size: Int
                                   ): Unit = {
    // ListBuffer#length is O(1) vs ListBuffer#size is O(N) (true in 2.9.x, fixed in 2.10.x)
    if (from.length >= size) {
      to ++= f(from.toList)
      from.clear
    }
  }

  def fetchBatchList[M <: MB, R, T, State](
                                            query: Query[M, R, State],
                                            batchSize: Int,
                                            readPreference: Option[ReadPreference] = None
                                          )(
                                            f: List[R] => List[T]
                                          )(
                                            implicit ev: ShardingOk[M, State],
                                            ev2: M !<:< MongoDisallowed
                                          ): List[T] = {
    if (optimizer.isEmptyQuery(query)) {
      Nil
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val rv = new ListBuffer[T]
      val buf = new ListBuffer[R]

      adapter.query(query, Some(batchSize), readPreference) { dbo =>
        buf += s.fromDocument(dbo)
        drainBufferList(buf, rv, f, batchSize)
      }
      drainBufferList(buf, rv, f, 1)

      rv.toList
    }
  }

  private def drainBufferSeq[A, B](
                                    from: ListBuffer[A],
                                    to: Builder[B, Vector[B]],
                                    f: Seq[A] => Seq[B],
                                    size: Int
                                  ): Unit = {
    // ListBuffer#length is O(1) vs ListBuffer#size is O(N) (true in 2.9.x, fixed in 2.10.x)
    if (from.length >= size) {
      to ++= f(from.toList)
      from.clear
    }
  }

  def fetchBatch[M <: MB, R, T, State](
                                        query: Query[M, R, State],
                                        batchSize: Int,
                                        readPreference: Option[ReadPreference] = None
                                      )(
                                        f: Seq[R] => Seq[T]
                                      )(
                                        implicit ev: ShardingOk[M, State],
                                        ev2: M !<:< MongoDisallowed
                                      ): Seq[T] = {
    if (optimizer.isEmptyQuery(query)) {
      Nil
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val rv = Vector.newBuilder[T]
      val buf = new ListBuffer[R]

      adapter.query(query, Some(batchSize), readPreference) { dbo =>
        buf += s.fromDocument(dbo)
        drainBufferSeq(buf, rv, f, batchSize)
      }
      drainBufferSeq(buf, rv, f, 1)

      rv.result
    }
  }

  def bulkDelete_!![M <: MB, State](query: Query[M, _, State],
                                    writeConcern: WriteConcern = defaultWriteConcern)
                                   (implicit ev1: Required[State, Unselected with Unlimited with Unskipped],
                                    ev2: ShardingOk[M, State], ev3: M !<:< MongoDisallowed): Unit = {
    if (optimizer.isEmptyQuery(query)) {
      ()
    } else {
      adapter.delete(query, writeConcern)
    }
  }

  def updateOne[M <: MB, State](
                                 query: ModifyQuery[M, State],
                                 writeConcern: WriteConcern = defaultWriteConcern
                               )(implicit ev: RequireShardKey[M, State]): Unit = {
    if (optimizer.isEmptyQuery(query)) {
      ()
    } else {
      adapter.modify(query, upsert = false, multi = false, writeConcern = writeConcern)
    }
  }

  def upsertOne[M <: MB, State](
                                 query: ModifyQuery[M, State],
                                 writeConcern: WriteConcern = defaultWriteConcern
                               )(implicit ev: RequireShardKey[M, State]): Unit = {
    if (optimizer.isEmptyQuery(query)) {
      ()
    } else {
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
  }

  def updateMulti[M <: MB, State](
                                   query: ModifyQuery[M, State],
                                   writeConcern: WriteConcern = defaultWriteConcern
                                 ): Unit = {
    if (optimizer.isEmptyQuery(query)) {
      ()
    } else {
      adapter.modify(query, upsert = false, multi = true, writeConcern = writeConcern)
    }
  }

  def findAndUpdateOne[M <: MB, R](
                                    query: FindAndModifyQuery[M, R],
                                    returnNew: Boolean = false,
                                    writeConcern: WriteConcern = defaultWriteConcern
                                  ): Option[R] = {
    None
    /*if (optimizer.isEmptyQuery(query)) {
      None
    } else {
      val s = readSerializer[M, R](query.query.meta, query.query.select)
      adapter.findAndModify(query, returnNew, upsert=false, remove=false)(s.fromDocument _)
    }*/
  }

  def findAndUpsertOne[M <: MB, R](
                                    query: FindAndModifyQuery[M, R],
                                    returnNew: Boolean = false,
                                    writeConcern: WriteConcern = defaultWriteConcern
                                  ): Option[R] = {
    None
    /*if (optimizer.isEmptyQuery(query)) {
      None
    } else {
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
    }*/
  }

  def findAndDeleteOne[M <: MB, R, State](
                                           query: Query[M, R, State],
                                           writeConcern: WriteConcern = defaultWriteConcern
                                         )(implicit ev: RequireShardKey[M, State]): Option[R] = {
    None
    /*if (optimizer.isEmptyQuery(query)) {
      None
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      val mod = FindAndModifyQuery(query, MongoModify(Nil))
      adapter.findAndModify(mod, returnNew=false, upsert=false, remove=true)(s.fromDocument _)
    }*/
  }

  def explain[M <: MB](query: Query[M, _, _]): String = {
    adapter.explain(query)
  }

  def iterate[S, M <: MB, R, State](query: Query[M, R, State],
                                    state: S,
                                    readPreference: Option[ReadPreference] = None)
                                   (handler: (S, Iter.Event[R]) => Iter.Command[S])
                                   (implicit ev: ShardingOk[M, State]): S = {
    /*if (optimizer.isEmptyQuery(query)) {
      handler(state, Iter.EOF).state
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      adapter.iterate(query, state, s.fromDocument _, readPreference)(handler)
    }*/
    state
  }

  def iterateBatch[S, M <: MB, R, State](query: Query[M, R, State],
                                         batchSize: Int,
                                         state: S,
                                         readPreference: Option[ReadPreference] = None)
                                        (handler: (S, Iter.Event[Seq[R]]) => Iter.Command[S])
                                        (implicit ev: ShardingOk[M, State]): S = {
    if (optimizer.isEmptyQuery(query)) {
      handler(state, Iter.EOF).state
    } else {
      val s = readSerializer[M, R](query.meta, query.select)
      //adapter.iterateBatch(query, batchSize, state, s.fromDocument _, readPreference)(handler)
      handler(state, Iter.EOF).state
    }
  }

  def insert[RecordType](record: RecordType, writeConcern: WriteConcern = defaultWriteConcern): RecordType = {
    //val s = writeSerializer(record)
    //val dbo = s.toDocument(record)
   // adapter.insert(record, dbo, writeConcern)
    record
  }

  /*def insertAll[RecordType <: RB](records: Seq[RecordType], writeConcern: WriteConcern = defaultWriteConcern): Seq[RecordType] = {
    records.headOption.foreach(record => {
      val s = writeSerializer(record)
      val dbos = records.map(s.toDocument)
      adapter.insertAll(record, dbos, writeConcern)
    })
    records
  }

  def remove[RecordType <: RB](record: RecordType, writeConcern: WriteConcern = defaultWriteConcern): RecordType = {
    val s = writeSerializer(record)
    val dbo = s.toDocument(record)
    adapter.remove(record, dbo, writeConcern)
    record
  }
  */
}
