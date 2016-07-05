package io.fsq.rogue


import java.util
import java.util.Collections

import io.fsq.rogue.index.UntypedMongoIndex
import io.fsq.rogue.MongoHelpers.MongoBuilder._
import io.fsq.rogue.{FindAndModifyQuery, ModifyQuery, Query, RogueSerializer}
import io.fsq.rogue.QueryHelpers._
import com.mongodb.client.result.{DeleteResult, UpdateResult}
import com.mongodb._
import com.mongodb.async.SingleResultCallback
import com.mongodb.async.client.{DistinctIterable, MongoCollection}
import com.mongodb.client.model._
import org.bson.Document
import org.bson.codecs.LongCodec
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}
import org.bson.conversions.Bson

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}
import scala.reflect.ClassTag


trait AsyncDBCollectionFactory[MB, RB] {
  def getDBCollection[M <: MB](query: Query[M, _, _]): MongoCollection[Document]

  def getPrimaryDBCollection[M <: MB](query: Query[M, _, _]): MongoCollection[Document]
  def getPrimaryDBCollection(record: RB): MongoCollection[Document]

  def getInstanceName[M <: MB](query: Query[M, _, _]): String
  def getInstanceName(record: RB): String

  // A set of of indexes, which are ordered lists of field names
  def getIndexes[M <: MB](query: Query[M, _, _]): Option[List[UntypedMongoIndex]]

}

trait HasFuture[T] {
  def future:Future[T]
}

class UnitCallback[T] extends SingleResultCallback[T] with HasFuture[Unit]{
  private[this] val promise = Promise[Unit]
  override def onResult(result: T, t: Throwable): Unit = {
      if(t == null) promise.success(())
      else promise.failure(t)
    }

  def future = promise.future
}

//specialized for Long to avoid conversion quirks
class PromiseLongCallbackBridge extends SingleResultCallback[java.lang.Long] {
  private[this] val promise = Promise[Long]()
  override def onResult(result: java.lang.Long, t: Throwable): Unit = {
    if (t == null) promise.success(result)
    else promise.failure(t)
  }
  def future = promise.future
}

class PromiseLongBooleanCallbackBridge extends SingleResultCallback[java.lang.Long] {
  val promise = Promise[Boolean]()
  override def onResult(result: java.lang.Long, t: Throwable): Unit = {
    if (t == null) promise.success(result.longValue() > 0)
    else promise.failure(t)
  }
  def future = promise.future
}

class PromiseArrayListAdapter[R] extends SingleResultCallback[java.util.Collection[R]] {
  val coll = new util.ArrayList[R]()
  private[this] val p = Promise[Seq[R]]
  //coll == result - by contract
  override def onResult(result: util.Collection[R], t: Throwable): Unit = {
    if (t == null) p.success(coll)
    else p.failure(t)
  }
  def future = p.future
}


class PromiseSingleResultAdapter[R] extends SingleResultCallback[java.util.Collection[R]]{
  val coll = new util.ArrayList[R](1)
  private[this] val p = Promise[Option[R]]
  //coll == result - by contract
  override def onResult(result: util.Collection[R], t: Throwable): Unit = {
    if (t == null) p.success(coll.headOption)
    else p.failure(t)
  }
  def future = p.future
}

class UpdateResultCallback extends SingleResultCallback[UpdateResult] with HasFuture[Unit] {
  private[this] val p = Promise[Unit]
  override def onResult(result: UpdateResult, t: Throwable): Unit = {
    if (t == null) p.success(())
    else p.failure(t)
  }
  def future = p.future
}

class UpdateResultCallbackWithRetry(retry: SingleResultCallback[UpdateResult] => Unit) extends SingleResultCallback[UpdateResult] with HasFuture[Unit]{
  private[this] val p = Promise[Unit]
  @volatile private[this] var retried = false
  override def onResult(result: UpdateResult, t: Throwable): Unit = {
    if (t == null) p.success(())
    else if(t != null && t.isInstanceOf[MongoException] && t.getCause.isInstanceOf[DuplicateKeyException] && !retried) {
      retried = true
      retry(this)
    } else p.failure(t)
  }
  def future = p.future
}

class SingleDocumentOptCallback[R](f: Document =>R) extends SingleResultCallback[Document] with HasFuture[Option[R]] {
  private[this] val p = Promise[Option[R]]
  override def onResult(result: Document, t: Throwable): Unit = {
    if(t == null) {
      if(result !=null) p.success(Option(f(result)))
      else p.success(None)
    } else p.failure(t)
  }
  def future = p.future
}

//upsert only
class SingleDocumentOptCallbackWithRetry[R](f: Document =>R)(retry: SingleDocumentOptCallbackWithRetry[R] => Unit) extends SingleResultCallback[Document] with HasFuture[Option[R]] {
  private [this] var retried = false
  val p = Promise[Option[R]]
  override def onResult(result: Document, t: Throwable): Unit = {
    if(t == null) {
      if(result !=null) p.success(Option(f(result)))
      else p.success(None)
    } else if(t != null && t.isInstanceOf[MongoException] && t.getCause.isInstanceOf[DuplicateKeyException] && !retried) {
      retried = true
      retry(this)
    } else p.failure(t)
  }
  def future = p.future
}



class MongoAsyncJavaDriverAdapter[MB, RB](dbCollectionFactory: AsyncDBCollectionFactory[MB, RB]) {

  def decoderFactoryFunc: (MB) => DBDecoderFactory = (m: MB) => DefaultDBDecoder.FACTORY

  def count[M <: MB](query: Query[M, _, _], readPreference: Option[ReadPreference]): Future[Long] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val condition: Bson = buildCondition(queryClause.condition)
    //we don't care for skip, limit in count - maybe we deviate from original, but it makes no sense anyways
    val coll = dbCollectionFactory.getDBCollection(query)
    val callback = new PromiseLongCallbackBridge()
    if(queryClause.lim.isDefined || queryClause.sk.isDefined) {
      val options = new CountOptions()
      queryClause.lim.map(options.limit(_))
      queryClause.sk.map(options.skip(_))
      coll.count(condition, options, callback)
    } else {
      coll.count(condition, callback)
    }
    callback.future
  }

  def exists[M <: MB](query: Query[M, _, _], readPreference: Option[ReadPreference]): Future[Boolean] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val condition: Bson = buildCondition(queryClause.condition)
    //we don't care for skip, limit in count - maybe we deviate from original, but it makes no sense anyways
    val coll = dbCollectionFactory.getDBCollection(query)
    val callback = new PromiseLongBooleanCallbackBridge()
    if(queryClause.lim.isDefined || queryClause.sk.isDefined) {
      val options = new CountOptions()
      queryClause.lim.map(options.limit(_))
      queryClause.sk.map(options.skip(_))
      coll.count(condition, options, callback)
    } else {
      coll.count(condition, callback)
    }
    callback.future
  }

  def countDistinct[M <: MB, R](query: Query[M, _, _],
                                key: String,
                                ct: ClassTag[R],
                                readPreference: Option[ReadPreference]): Future[Long] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)
    val coll = dbCollectionFactory.getDBCollection(query)
    val rClass = ct.runtimeClass.asInstanceOf[Class[R]]
    //a dummy, but result needs it
    val arr = new util.ArrayList[R]
    val p = Promise[Long]
    val pa = new SingleResultCallback[java.util.Collection[R]]() {
      override def onResult(result: java.util.Collection[R], t: Throwable): Unit = {
        if (t == null) p.success(result.size())
        else p.failure(t)
      }
    }
    coll.distinct(key, cnd, rClass).into(arr.asInstanceOf[util.Collection[R]], pa)
    p.future
  }


  def distinct[M <: MB, R](query: Query[M, _, _],
                           key: String,
                           ct: ClassTag[R],
                           readPreference: Option[ReadPreference]): Future[Seq[R]] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)
    val coll = dbCollectionFactory.getDBCollection(query)
    val rClass = ct.runtimeClass.asInstanceOf[Class[R]]
    val pa = new PromiseArrayListAdapter[R]()
    coll.distinct[R](key, cnd, rClass).into(pa.coll, pa)
    pa.future
  }


  def find[M <: MB, R](query: Query[M, _, _], serializer: RogueReadSerializer[R]): Future[Seq[R]] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)
    val coll = dbCollectionFactory.getDBCollection(query)
    val sel = queryClause.select.map(buildSelect).getOrElse(BasicDBObjectBuilder.start.get.asInstanceOf[BasicDBObject])
    val ord = queryClause.order.map(buildOrder)

    //check if serializer will work - quite possible that no, and separate mapper from Document -> R will be needed
    val adaptedSerializer = new com.mongodb.Function[Document,R]{
      override def apply(d: Document):R = serializer.fromDocument(d)
    }
    val pa = new PromiseArrayListAdapter[R]()
    //sort, hints
    val cursor = coll.find(cnd).projection(sel)
    queryClause.lim.foreach(cursor.limit _)
    queryClause.sk.foreach(cursor.skip _)
    ord.foreach(cursor.sort _)
    cursor.map(adaptedSerializer).into(pa.coll, pa)
    pa.future
  }

  def fineOne[M <: MB, R](query: Query[M, _, _], serializer: RogueReadSerializer[R]): Future[Option[R]] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)
    val coll = dbCollectionFactory.getDBCollection(query)
    val sel = queryClause.select.map(buildSelect).getOrElse(BasicDBObjectBuilder.start.get.asInstanceOf[BasicDBObject])
    val ord = queryClause.order.map(buildOrder)
    //check if serializer will work - quite possible that no, and separate mapper from Document -> R will be needed
    val adaptedSerializer = new com.mongodb.Function[Document,R]{
      override def apply(d: Document):R = serializer.fromDocument(d)
    }
    val oneP = new PromiseSingleResultAdapter[R]()
    val cursor = coll.find(cnd).projection(sel)
    queryClause.lim.foreach(cursor.limit _)
    queryClause.sk.foreach(cursor.skip _)
    ord.foreach(cursor.sort _)

    cursor.map(adaptedSerializer).into(oneP.coll, oneP)
    oneP.future
  }

  def foreach[M <: MB, R](query: Query[M, _, _], f: Document => Unit): Future[Unit] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)
    val coll = dbCollectionFactory.getDBCollection(query)
    val callback = new UnitCallback[Void]
    coll.find(cnd).forEach(new Block[Document]{
      override def apply(t: Document): Unit = f(t)
    }, callback)
    callback.future
  }

  def delete[M <: MB](query: Query[M, _, _],
                      writeConcern: WriteConcern): Future[Unit] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)
    val coll = dbCollectionFactory.getPrimaryDBCollection(query)
    val callback = new UnitCallback[DeleteResult]
    coll.deleteMany(cnd, callback)
    callback.future
  }

  def modify[M <: MB](mod: ModifyQuery[M, _],
                      upsert: Boolean,
                      multi: Boolean,
                      writeConcern: WriteConcern): Future[Unit] = {
    val modClause = transformer.transformModify(mod)
    validator.validateModify(modClause, dbCollectionFactory.getIndexes(modClause.query))
    if (!modClause.mod.clauses.isEmpty) {
      val q = buildCondition(modClause.query.condition)
      val m = buildModify(modClause.mod)
      val coll = dbCollectionFactory.getPrimaryDBCollection(modClause.query)
      val updateOptions = new UpdateOptions().upsert(upsert)
      val updater: (SingleResultCallback[UpdateResult]) => Unit = if(multi) {
        coll.updateMany(q,m, updateOptions , _)
      } else {
        coll.updateOne(q,m, updateOptions, _)
      }
      val callback = if(upsert) new UpdateResultCallbackWithRetry(updater)
      else new UpdateResultCallback
      updater(callback)
      callback.future
    } else Future.successful(())
    //else clause = no modify, automatic success ... strange but true

  }

  def findAndModify[M <: MB, R](mod: FindAndModifyQuery[M, R],
                                returnNew: Boolean,
                                upsert: Boolean,
                                remove: Boolean)
                               (f: Document => R): Future[Option[R]] = {
    val modClause = transformer.transformFindAndModify(mod)
    validator.validateFindAndModify(modClause, dbCollectionFactory.getIndexes(modClause.query))
    if (!modClause.mod.clauses.isEmpty || remove) {
      val query = modClause.query
      val cnd = buildCondition(query.condition)
      val ord = query.order.map(buildOrder)
      val m = buildModify(modClause.mod)
      val coll = dbCollectionFactory.getPrimaryDBCollection(query)
      val retDoc = if(returnNew) ReturnDocument.AFTER else ReturnDocument.BEFORE
      val updater: (SingleResultCallback[Document]) => Unit = if(remove) {
        val opts = new FindOneAndDeleteOptions()
        ord.map(dbo => opts.sort(dbo))
        coll.findOneAndDelete(cnd, opts, _)
      } else {
        val opts = new FindOneAndUpdateOptions().returnDocument(retDoc).upsert(upsert)
        ord.map(dbo => opts.sort(dbo))
        coll.findOneAndUpdate(cnd, m, opts, _)
      }
      val callback = if(upsert) new SingleDocumentOptCallbackWithRetry[R](f)(updater)
      else new SingleDocumentOptCallback[R](f)
      updater(callback)
      callback.future
    }
    else Future.successful(None)
  }

  def insert(record: RB, dbo: Document, writeConcern: WriteConcern): Future[Unit] = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(record)
    val callback = new UnitCallback[Void]
    collection.insertOne(dbo, callback)
    callback.future
  }

  def insertAll(record: RB, dbos: Seq[Document], writeConcern: WriteConcern): Future[Unit] = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(record)
    val callback = new UnitCallback[Void]
    collection.insertMany(dbos, callback)
    callback.future
  }

}
