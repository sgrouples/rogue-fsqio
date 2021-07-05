package me.sgrouples.rogue.cc

import com.mongodb.{BasicDBObject, BasicDBObjectBuilder}

import java.util
import org.mongodb.scala._
import org.mongodb.scala.model._
import io.fsq.rogue.MongoHelpers.MongoBuilder._
import io.fsq.rogue.{FindAndModifyQuery, ModifyQuery, Query}
import io.fsq.rogue.QueryHelpers._
import io.fsq.rogue.index.UntypedMongoIndex
import org.bson.{BsonDocument, BsonReader, BsonWriter}
import org.mongodb.scala.result.{DeleteResult, InsertManyResult, InsertOneResult}

import scala.collection.mutable.ListBuffer
//import org.bson.{ Document, BsonReader, BsonWriter }
import org.bson.conversions.Bson
import scala.jdk.CollectionConverters._
import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.reflect.ClassTag
import scala.util.{ Failure, Success, Try }
import com.mongodb.ErrorCategory._
import org.bson.codecs.{ Codec, DecoderContext, EncoderContext, RawDocumentCodec }
import org.bson.codecs.configuration.{ CodecConfigurationException, CodecRegistries }
import org.bson.types.ObjectId
import org.reactivestreams.Publisher

import scala.reflect._
//rename to reactive
trait AsyncBsonDBCollectionFactory[MB] {

  def getDBCollection[M <: MB](query: Query[M, _, _])(implicit dba: MongoDatabase): MongoCollection[Document]

  ///def getReactiveCollection[M <: MB](query: Query[M, _, _])(implicit dba: MongoDatabase): ReactiveMongoCollection[Document]

  def getPrimaryDBCollection[M <: MB](query: Query[M, _, _])(implicit dba: MongoDatabase): MongoCollection[Document]

  def getInstanceName[M <: MB](query: Query[M, _, _])(implicit dba: MongoDatabase): String

  // A set of of indexes, which are ordered lists of field names
  def getIndexes[M <: MB](query: Query[M, _, _])(implicit dba: MongoDatabase): Option[Seq[UntypedMongoIndex]]

}

class MongoAsyncBsonJavaDriverAdapter[MB](dbCollectionFactory: AsyncBsonDBCollectionFactory[MB]) {

  //def decoderFactoryFunc: (MB) => DBDecoderFactory = (m: MB) => DefaultDBDecoder.FACTORY

  private def getDBCollection[M <: MB, R](
    query: Query[M, _, _],
    readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): MongoCollection[Document] = {
    val c = dbCollectionFactory.getDBCollection(query)
    (readPreference.toSeq ++ query.readPreference.toSeq).headOption.fold(c)(c.withReadPreference)
  }


  def count[M <: MB](query: Query[M, _, _], readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): Future[Long] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val condition: Bson = buildCondition(queryClause.condition)
    //we don't care for skip, limit in count - maybe we deviate from original, but it makes no sense anyways
    val coll = getDBCollection(query, readPreference)
    //val callback = new PromiseLongCallbackBridge()
    val obs: SingleObservable[Long] = if (queryClause.lim.isDefined || queryClause.sk.isDefined) {
      val options = new CountOptions()
      queryClause.lim.map(options.limit(_))
      queryClause.sk.map(options.skip(_))
      coll.countDocuments(condition, options)
    } else {
      coll.countDocuments(condition)
    }
    //TODO - adapt to FS2 / Source and so on in modules
    obs.toFuture()
  }

  def exists[M <: MB](query: Query[M, _, _], readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): Future[Boolean] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val condition: Bson = buildCondition(queryClause.condition)
    //we don't care for skip, limit in count - maybe we deviate from original, but it makes no sense anyways
    val coll = getDBCollection(query, readPreference)
    val obs = if (queryClause.lim.isDefined || queryClause.sk.isDefined) {
      val options = new CountOptions()
      queryClause.lim.map(options.limit(_))
      queryClause.sk.map(options.skip(_))
      coll.countDocuments(condition, options)
    } else {
      coll.countDocuments(condition)
    }
    val p = Promise[Boolean]()
    obs.subscribe(cnt => p.success(cnt > 0L), err => p.failure(err), () => p.trySuccess(false))
    p.future
  }

  def countDistinct[M <: MB, R: ClassTag](
    query: Query[M, _, _],
    key: String,
    readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): Future[Long] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val p = Promise[Long]()
    val arr = new ListBuffer[R]
    coll.distinct[R](key, cnd).subscribe(r => arr+=(r), err => p.failure(err), () => p.trySuccess(arr.length))
    p.future
  }

  def distinct[M <: MB, R](
    query: Query[M, _, _],
    key: String,
    ct: ClassTag[R], // implicit ct ?
    readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): Future[Seq[R]] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    coll.distinct[R](key, cnd).toFuture()
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

    //check if serializer will work - quite possible that no, and separate mapper from Document-> R will be needed
    val adaptedSerializer = new com.mongodb.Function[Document, R] {
      override def apply(d: Document): R = serializer.fromDocument(d)
    }
    //sort, hints
    val cursor = coll.find(cnd).projection(sel)
    queryClause.lim.foreach(cursor.limit _)
    queryClause.sk.foreach(cursor.skip _)
    ord.foreach(cursor.sort _)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    cursor.map(adaptedSerializer).into(pa.coll, pa)
    pa.future
  }

  private[this] val rawDocumentCodec = new RawDocumentCodec

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
        val bsonDoc = rawDocumentCodec.decode(reader, decoderContext)
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
    //check if serializer will work - quite possible that no, and separate mapper from Document-> R will be needed

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
    //check if serializer will work - quite possible that no, and separate mapper from Document-> R will be needed
    val adaptedSerializer = new com.mongodb.Function[Document, R] {
      override def apply(d: Document): R = serializer.fromDocument(d)
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
    f: Document => Unit,
    readPreference: Option[ReadPreference] = None)(implicit dba: MongoDatabase): Future[Unit] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val cursor = coll.find(cnd)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    cursor.forEach((t: Document) => f(t), callback)
    callback.future
  }

  def delete[M <: MB](
    query: Query[M, _, _],
    writeConcern: WriteConcern)(implicit dba: MongoDatabase): Future[DeleteResult] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)
    val coll = dbCollectionFactory.getPrimaryDBCollection(query)
    coll.deleteMany(cnd).toFuture()
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
    remove: Boolean)(f: Document => Option[R])(implicit dba: MongoDatabase): Future[Option[R]] = {
    val modClause = transformer.transformFindAndModify(mod)
    validator.validateFindAndModify(modClause, dbCollectionFactory.getIndexes(modClause.query))
    if (!modClause.mod.clauses.isEmpty || remove) {
      val query = modClause.query
      val cnd: Bson = buildCondition(query.condition)
      val ord = query.order.map(buildOrder)
      val m: Bson = buildModify(modClause.mod)
      val coll = dbCollectionFactory.getPrimaryDBCollection(query)
      val retDoc = if (returnNew) ReturnDocument.AFTER else ReturnDocument.BEFORE
      val updater: (SingleResultCallback[Document]) => Unit = if (remove) {
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

  def insertOne[M <: MB, R](query: Query[M, R, _], doc: Document, writeConcern: WriteConcern)(implicit dba: MongoDatabase): Future[InsertOneResult] = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(query)
    collection.insertOne(doc).toFuture()
  }

  def insertMany[M <: MB, R](query: Query[M, R, _], docs: Seq[Document], writeConcern: WriteConcern)(implicit dba: MongoDatabase): Future[InsertManyResult] = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(query)
    collection.insertMany(docs).toFuture()
  }

  def replaceOne[M <: MB, R](query: Query[M, R, _], doc: Document, upsert: Boolean, writeConcern: WriteConcern)(implicit dba: MongoDatabase): Future[Unit] = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(query)
    val filter = new BsonDocument("_id", doc.getObjectId("_id"))
    collection.replaceOne(filter, doc, new UpdateOptions().upsert(upsert))
    callback.future
  }

  private def queryToFindIterable[M <: MB, R](query: Query[M, R, _], readPreference: Option[ReadPreference])(implicit dba: MongoDatabase): FindIterable[Document] = {
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
