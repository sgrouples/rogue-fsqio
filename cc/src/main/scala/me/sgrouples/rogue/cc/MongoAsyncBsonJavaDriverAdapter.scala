package me.sgrouples.rogue.cc

import java.util

import com.mongodb._
import com.mongodb.async.{ AsyncBatchCursor, SingleResultCallback }
import com.mongodb.async.client.{ FindIterable, MongoCollection, MongoDatabase }
import com.mongodb.client.model._
import com.mongodb.client.result.{ DeleteResult, UpdateResult }
import com.mongodb.reactivestreams.client.{ MongoCollection => ReactiveMongoCollection }
import io.fsq.rogue.MongoHelpers.MongoBuilder._
import io.fsq.rogue.{ FindAndModifyQuery, ModifyQuery, Query }
import io.fsq.rogue.QueryHelpers._
import io.fsq.rogue.index.UntypedMongoIndex
import org.bson.{ BsonDocument, BsonReader, BsonWriter }
import org.bson.conversions.Bson
import scala.collection.compat._
import scala.jdk.CollectionConverters._
//import scala.collection.JavaConverters._
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.reflect.ClassTag
import scala.util.{ Failure, Success, Try }
import com.mongodb.ErrorCategory._
import org.bson.codecs.{ Codec, DecoderContext, EncoderContext, RawBsonDocumentCodec }
import org.bson.codecs.configuration.{ CodecConfigurationException, CodecRegistries }
import org.bson.types.ObjectId
import org.reactivestreams.Publisher

import scala.reflect._

trait AsyncBsonDBCollectionFactory[MB] {

  def getDBCollection[M <: MB](query: Query[M, _, _])(implicit dba: MongoDatabase): MongoCollection[BsonDocument]

  def getReactiveCollection[M <: MB](query: Query[M, _, _])(implicit dba: MongoDatabase): ReactiveMongoCollection[BsonDocument]

  def getPrimaryDBCollection[M <: MB](query: Query[M, _, _])(implicit dba: MongoDatabase): MongoCollection[BsonDocument]

  def getInstanceName[M <: MB](query: Query[M, _, _])(implicit dba: MongoDatabase): String

  // A set of of indexes, which are ordered lists of field names
  def getIndexes[M <: MB](query: Query[M, _, _])(implicit dba: MongoDatabase): Option[Seq[UntypedMongoIndex]]

}

trait HasFuture[T] {
  def future: Future[T]
}

class UnitCallback[T] extends SingleResultCallback[T] with HasFuture[Unit] {
  private[this] val promise = Promise[Unit]

  override def onResult(result: T, t: Throwable): Unit = {
    if (t == null) promise.success(())
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
    if (t == null) p.success(coll.asScala.toSeq) //immutable Seq - may be slow
    else p.failure(t)
  }

  def future = p.future
}

class PromiseSingleResultAdapter[R] extends SingleResultCallback[java.util.Collection[R]] {
  val coll = new util.ArrayList[R](1)
  private[this] val p = Promise[Option[R]]

  //coll == result - by contract
  override def onResult(result: util.Collection[R], t: Throwable): Unit = {
    if (t == null) p.success(coll.asScala.headOption)
    else p.failure(t)
  }

  def future = p.future
}

class PromiseSingleValueAdapter[V] extends SingleResultCallback[V] {
  private[this] val p = Promise[V]
  override def onResult(result: V, t: Throwable): Unit = {
    if (t == null) p.success(result)
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

class UpdateResultCallbackReturning extends SingleResultCallback[UpdateResult] with HasFuture[UpdateResult] {
  private[this] val p = Promise[UpdateResult]

  override def onResult(result: UpdateResult, t: Throwable): Unit = {
    if (t == null) p.success(result)
    else p.failure(t)
  }
  def future = p.future
}

class UpdateResultCallbackWithRetry(retry: SingleResultCallback[UpdateResult] => Unit) extends SingleResultCallback[UpdateResult] with HasFuture[Unit] {
  private[this] val p = Promise[Unit]
  @volatile private[this] var retried = false

  override def onResult(result: UpdateResult, t: Throwable): Unit = {
    t match {
      case null => p.success(())
      case e: MongoException if fromErrorCode(e.getCode) == DUPLICATE_KEY && !retried =>
        retried = true
        retry(this)
      case _ => p.failure(t)
    }
  }

  def future = p.future
}

class UpdateResultCallbackWithRetryReturning(retry: SingleResultCallback[UpdateResult] => Unit) extends SingleResultCallback[UpdateResult] with HasFuture[UpdateResult] {
  private[this] val p = Promise[UpdateResult]
  @volatile private[this] var retried = false

  override def onResult(result: UpdateResult, t: Throwable): Unit = {
    t match {
      case null => p.success(result)
      case e: MongoException if fromErrorCode(e.getCode) == DUPLICATE_KEY && !retried =>
        retried = true
        retry(this)
      case _ => p.failure(t)
    }
  }

  def future = p.future
}

class SingleDocumentOptCallback[R](f: BsonDocument => Option[R]) extends SingleResultCallback[BsonDocument] with HasFuture[Option[R]] {
  private[this] val p = Promise[Option[R]]

  override def onResult(result: BsonDocument, t: Throwable): Unit = {
    if (t == null) {
      if (result != null) p.complete(Try(f(result)))
      else p.success(None)
    } else p.failure(t)
  }

  def future = p.future
}

//upsert only
class SingleDocumentOptCallbackWithRetry[R](f: BsonDocument => Option[R])(retry: SingleDocumentOptCallbackWithRetry[R] => Unit) extends SingleResultCallback[BsonDocument] with HasFuture[Option[R]] {
  private[this] var retried = false
  val p = Promise[Option[R]]

  override def onResult(result: BsonDocument, t: Throwable): Unit = {
    t match {
      case null if result != null => p.complete(Try(f(result)))
      case null => p.success(None)
      case e: MongoException if fromErrorCode(e.getCode) == DUPLICATE_KEY && !retried =>
        retried = true
        retry(this)
      case _ => p.failure(t)
    }
  }

  def future = p.future
}

class BatchingCallback[R, T](r: RogueBsonRead[R], f: Seq[R] => Future[Seq[T]])(implicit ec: ExecutionContext) extends SingleResultCallback[AsyncBatchCursor[BsonDocument]] with HasFuture[Seq[T]] {
  private val p = Promise[Seq[T]]
  private val resBuilder = Seq.newBuilder[T]

  class ResultListCallback(batchCursor: AsyncBatchCursor[BsonDocument]) extends SingleResultCallback[java.util.List[BsonDocument]] {
    override def onResult(docs: util.List[BsonDocument], t: Throwable): Unit = {
      if (t != null) {
        batchCursor.close()
        p.failure(t)
      } else if (docs == null) {
        batchCursor.close()
        p.success(resBuilder.result())
      } else {
        f(docs.asScala.toSeq.map(r.fromDocument)).andThen {
          case Success(resT) =>
            resBuilder ++= resT
            batchCursor.next(this)
          case Failure(t) =>
            batchCursor.close()
            p.failure(t)
        }
      }
    }
  }

  override def onResult(batchCursor: AsyncBatchCursor[BsonDocument], t: Throwable): Unit = {
    if (t != null) {
      p.failure(t)
    } else if (batchCursor == null) {
      p.success(resBuilder.result())
    } else {
      //batchCursor.next can throw  java.util.NoSuchElementException, thats why Try is here
      Try {
        val resultCB = new ResultListCallback(batchCursor)
        batchCursor.next(resultCB)
      }.recover {
        case e =>
          batchCursor.close()
          p.failure(e)
      }
    }
  }

  override def future: Future[Seq[T]] = p.future
}

class MongoAsyncBsonJavaDriverAdapter[MB](dbCollectionFactory: AsyncBsonDBCollectionFactory[MB]) {

  def decoderFactoryFunc: (MB) => DBDecoderFactory = (m: MB) => DefaultDBDecoder.FACTORY

  private def getDBCollection[M <: MB, R](
    query: Query[M, _, _],
    readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): MongoCollection[BsonDocument] = {
    val c = dbCollectionFactory.getDBCollection(query)
    (readPreference.toSeq ++ query.readPreference.toSeq).headOption.fold(c)(c.withReadPreference)
  }

  private def getReactiveCollection[M <: MB, R](
    query: Query[M, _, _],
    readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): ReactiveMongoCollection[BsonDocument] = {
    val c = dbCollectionFactory.getReactiveCollection(query)
    (readPreference.toSeq ++ query.readPreference.toSeq).headOption.fold(c)(c.withReadPreference)
  }

  def count[M <: MB](query: Query[M, _, _], readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): Future[Long] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val condition: Bson = buildCondition(queryClause.condition)
    //we don't care for skip, limit in count - maybe we deviate from original, but it makes no sense anyways
    val coll = getDBCollection(query, readPreference)
    val callback = new PromiseLongCallbackBridge()
    if (queryClause.lim.isDefined || queryClause.sk.isDefined) {
      val options = new CountOptions()
      queryClause.lim.map(options.limit(_))
      queryClause.sk.map(options.skip(_))
      coll.count(condition, options, callback)
    } else {
      coll.count(condition, callback)
    }
    callback.future
  }

  def exists[M <: MB](query: Query[M, _, _], readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): Future[Boolean] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val condition: Bson = buildCondition(queryClause.condition)
    //we don't care for skip, limit in count - maybe we deviate from original, but it makes no sense anyways
    val coll = getDBCollection(query, readPreference)
    val callback = new PromiseLongBooleanCallbackBridge()
    if (queryClause.lim.isDefined || queryClause.sk.isDefined) {
      val options = new CountOptions()
      queryClause.lim.map(options.limit(_))
      queryClause.sk.map(options.skip(_))
      coll.count(condition, options, callback)
    } else {
      coll.count(condition, callback)
    }
    callback.future
  }

  def countDistinct[M <: MB, R](
    query: Query[M, _, _],
    key: String,
    ct: ClassTag[R],
    readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): Future[Long] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
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

  def distinct[M <: MB, R](
    query: Query[M, _, _],
    key: String,
    ct: ClassTag[R],
    readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): Future[Seq[R]] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val rClass = ct.runtimeClass.asInstanceOf[Class[R]]
    val pa = new PromiseArrayListAdapter[R]()
    coll.distinct[R](key, cnd, rClass).into(pa.coll, pa)
    pa.future
  }

  def find[M <: MB, R](
    query: Query[M, _, _],
    serializer: RogueBsonRead[R],
    readPreference: Option[ReadPreference] = None)(implicit dba: MongoDatabase): Future[Seq[R]] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val sel: Bson = queryClause.select.map(buildSelect).getOrElse(BasicDBObjectBuilder.start.get.asInstanceOf[BasicDBObject])
    val ord = queryClause.order.map(buildOrder)

    //check if serializer will work - quite possible that no, and separate mapper from BsonDocument-> R will be needed
    val adaptedSerializer = new com.mongodb.Function[BsonDocument, R] {
      override def apply(d: BsonDocument): R = serializer.fromDocument(d)
    }
    val pa = new PromiseArrayListAdapter[R]()
    //sort, hints
    val cursor = coll.find(cnd).projection(sel)
    queryClause.lim.foreach(cursor.limit _)
    queryClause.sk.foreach(cursor.skip _)
    ord.foreach(cursor.sort _)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    cursor.map(adaptedSerializer).into(pa.coll, pa)
    pa.future
  }

  private[this] val rawBsonDocumentCodec = new RawBsonDocumentCodec

  def findPublisher[M <: MB, R: ClassTag](
    query: Query[M, _, _],
    readSerializer: RogueBsonRead[R],
    batchSize: Int,
    readPreference: Option[ReadPreference] = None)(implicit dba: MongoDatabase): Publisher[R] = {
    val classR = classTag[R].runtimeClass.asInstanceOf[Class[R]]
    val queryClause = transformer.transformQuery(query)

    val cnd: Bson = buildCondition(queryClause.condition)
    val sel: Bson = queryClause.select.map(buildSelect).getOrElse(BasicDBObjectBuilder.start.get.asInstanceOf[BasicDBObject])
    val ord = queryClause.order.map(buildOrder)
    val baseColl = getReactiveCollection(query, readPreference)

    val encoder = Try {
      baseColl.getCodecRegistry.get(classR)
    }

    val rCodec = new Codec[R] {

      override def decode(reader: BsonReader, decoderContext: DecoderContext): R = {
        val bsonDoc = rawBsonDocumentCodec.decode(reader, decoderContext)
        readSerializer.fromDocument(bsonDoc)
      }

      //don't care - only read
      override def encode(writer: BsonWriter, value: R, encoderContext: EncoderContext): Unit = {
        encoder.map(_.encode(writer, value, encoderContext)).getOrElse(
          throw new IllegalStateException(s"Encoder not implemented encoder for ${classR} - only reader in fetch - query was ${cnd} / ${sel}"))
      }

      override def getEncoderClass: Class[R] = classR.asInstanceOf[Class[R]]
    }
    val singleR = CodecRegistries.fromCodecs(rCodec)
    val cr = CodecRegistries.fromRegistries(singleR, baseColl.getCodecRegistry)

    val coll = baseColl.withCodecRegistry(cr)
    //check if serializer will work - quite possible that no, and separate mapper from BsonDocument-> R will be needed

    // val pa = new PromiseArrayListAdapter[R]()
    //sort, hints

    val cursor = coll.find(cnd, classR).projection(sel)
    queryClause.lim.foreach(cursor.limit _)
    queryClause.sk.foreach(cursor.skip _)
    ord.foreach(cursor.sort _)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    //cursor.map(adaptedSerializer).into(pa.coll, pa)
    cursor.batchSize(batchSize)
  }

  def fineOne[M <: MB, R](
    query: Query[M, _, _],
    serializer: RogueBsonRead[R],
    readPreference: Option[ReadPreference] = None)(implicit dba: MongoDatabase): Future[Option[R]] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val sel: Bson = queryClause.select.map(buildSelect).getOrElse(BasicDBObjectBuilder.start.get.asInstanceOf[BasicDBObject])
    val ord = queryClause.order.map(buildOrder)
    //check if serializer will work - quite possible that no, and separate mapper from BsonDocument-> R will be needed
    val adaptedSerializer = new com.mongodb.Function[BsonDocument, R] {
      override def apply(d: BsonDocument): R = serializer.fromDocument(d)
    }
    val oneP = new PromiseSingleResultAdapter[R]()
    val cursor = coll.find(cnd).projection(sel)
    queryClause.lim.foreach(cursor.limit _)
    queryClause.sk.foreach(cursor.skip _)
    ord.foreach(cursor.sort _)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    cursor.map(adaptedSerializer).into(oneP.coll, oneP)
    oneP.future
  }

  def foreach[M <: MB, R](
    query: Query[M, _, _],
    f: BsonDocument => Unit,
    readPreference: Option[ReadPreference] = None)(implicit dba: MongoDatabase): Future[Unit] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val callback = new UnitCallback[Void]
    val cursor = coll.find(cnd)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    cursor.forEach((t: BsonDocument) => f(t), callback)
    callback.future
  }

  def delete[M <: MB](
    query: Query[M, _, _],
    writeConcern: WriteConcern)(implicit dba: MongoDatabase): Future[Unit] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)
    val coll = dbCollectionFactory.getPrimaryDBCollection(query)
    val callback = new UnitCallback[DeleteResult]
    coll.deleteMany(cnd, callback)
    callback.future
  }

  private[this] def modifyInternal[M <: MB, RT](
    mod: ModifyQuery[M, _],
    upsert: Boolean,
    multi: Boolean,
    writeConcern: WriteConcern, emptyRT: RT,
    callbackCreator: (SingleResultCallback[UpdateResult] => Unit) => SingleResultCallback[UpdateResult] with HasFuture[RT])(implicit dba: MongoDatabase): Future[RT] = {
    val modClause = transformer.transformModify(mod)
    validator.validateModify(modClause, dbCollectionFactory.getIndexes(modClause.query))
    if (!modClause.mod.clauses.isEmpty) {
      val q: Bson = buildCondition(modClause.query.condition)
      val m: Bson = buildModify(modClause.mod)
      val coll = dbCollectionFactory.getPrimaryDBCollection(modClause.query)
      val updateOptions = new UpdateOptions().upsert(upsert)
      val updater: (SingleResultCallback[UpdateResult]) => Unit = if (multi) {
        coll.updateMany(q, m, updateOptions, _)
      } else {
        coll.updateOne(q, m, updateOptions, _)
      }
      val callback = callbackCreator(updater)
      updater(callback)
      callback.future
    } else {
      Future.successful(emptyRT)
    }
  }

  def modify[M <: MB](
    mod: ModifyQuery[M, _],
    upsert: Boolean,
    multi: Boolean,
    writeConcern: WriteConcern)(implicit dba: MongoDatabase): Future[Unit] = {
    modifyInternal[M, Unit](mod, upsert, multi, writeConcern,
      (),
      (updater) => if (upsert) new UpdateResultCallbackWithRetry(updater)
      else new UpdateResultCallback)
  }

  /**
   *
   * @param mod
   * @param upsert
   * @param multi
   * @param writeConcern
   * @param dba
   * @tparam M
   * @return UpdateResult if writeConcern >= Acknowledged, so writer can learn about relevant data (number of updated, created etc)
   */
  def modifyRet[M <: MB](
    mod: ModifyQuery[M, _],
    upsert: Boolean,
    multi: Boolean,
    writeConcern: WriteConcern)(implicit dba: MongoDatabase): Future[UpdateResult] = {
    modifyInternal[M, UpdateResult](mod, upsert, multi, writeConcern,
      UpdateResult.unacknowledged(),
      (updater) => if (upsert) new UpdateResultCallbackWithRetryReturning(updater)
      else new UpdateResultCallbackReturning)
  }

  def findAndModify[M <: MB, R](
    mod: FindAndModifyQuery[M, R],
    returnNew: Boolean,
    upsert: Boolean,
    remove: Boolean)(f: BsonDocument => Option[R])(implicit dba: MongoDatabase): Future[Option[R]] = {
    val modClause = transformer.transformFindAndModify(mod)
    validator.validateFindAndModify(modClause, dbCollectionFactory.getIndexes(modClause.query))
    if (!modClause.mod.clauses.isEmpty || remove) {
      val query = modClause.query
      val cnd: Bson = buildCondition(query.condition)
      val ord = query.order.map(buildOrder)
      val m: Bson = buildModify(modClause.mod)
      val coll = dbCollectionFactory.getPrimaryDBCollection(query)
      val retDoc = if (returnNew) ReturnDocument.AFTER else ReturnDocument.BEFORE
      val updater: (SingleResultCallback[BsonDocument]) => Unit = if (remove) {
        val opts = new FindOneAndDeleteOptions()
        ord.map(dbo => opts.sort(dbo))
        coll.findOneAndDelete(cnd, opts, _)
      } else {
        val opts = new FindOneAndUpdateOptions().returnDocument(retDoc).upsert(upsert)
        ord.map(dbo => opts.sort(dbo))
        coll.findOneAndUpdate(cnd, m, opts, _)
      }
      val callback = if (upsert) new SingleDocumentOptCallbackWithRetry[R](f)(updater)
      else new SingleDocumentOptCallback[R](f)
      updater(callback)
      callback.future
    } else Future.successful(None)
  }

  def insertOne[M <: MB, R](query: Query[M, R, _], doc: BsonDocument, writeConcern: WriteConcern)(implicit dba: MongoDatabase): Future[Unit] = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(query)
    val callback = new UnitCallback[Void]
    collection.insertOne(doc, callback)
    callback.future
  }

  def insertMany[M <: MB, R](query: Query[M, R, _], docs: Seq[BsonDocument], writeConcern: WriteConcern)(implicit dba: MongoDatabase): Future[Unit] = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(query)
    val callback = new UnitCallback[Void]
    collection.insertMany(docs.asJava, callback)
    callback.future
  }

  def replaceOne[M <: MB, R](query: Query[M, R, _], doc: BsonDocument, upsert: Boolean, writeConcern: WriteConcern)(implicit dba: MongoDatabase): Future[Unit] = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(query)
    val callback = new UpdateResultCallback
    val filter = new BsonDocument("_id", doc.get("_id"))
    collection.replaceOne(filter, doc, new UpdateOptions().upsert(upsert), callback)
    callback.future
  }

  private def queryToFindIterable[M <: MB, R](query: Query[M, R, _], readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): FindIterable[BsonDocument] = {
    val cnd: Bson = buildCondition(query.condition)
    val coll = getDBCollection(query, readPreference)
    val sel: Bson = query.select.map(buildSelect).getOrElse(BasicDBObjectBuilder.start.get.asInstanceOf[BasicDBObject])
    val ord = query.order.map(buildOrder)
    val cursor = coll.find(cnd).projection(sel)
    //mongo driver has side-effecting fuctions for limit, skip, hint
    query.lim.foreach(cursor.limit _)
    query.sk.foreach(cursor.skip _)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    ord.foreach(cursor.sort _)
    cursor
  }

  def batch[M <: MB, R, T](query: Query[M, R, _], serializer: RogueBsonRead[R], f: Seq[R] => Future[Seq[T]], batchSize: Int,
    readPreference: Option[ReadPreference])(implicit dba: MongoDatabase, ec: ExecutionContext): Future[Seq[T]] = {
    val fi = queryToFindIterable(query, readPreference)
    val batchCB = new BatchingCallback(serializer, f)
    fi.batchSize(batchSize).batchCursor(batchCB)
    batchCB.future
  }

}
