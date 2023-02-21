package me.sgrouples.rogue.cc

import com.mongodb.client.result.UpdateResult
import com.mongodb.{BasicDBObject, BasicDBObjectBuilder}
import org.mongodb.scala._
import org.mongodb.scala.model._
import io.fsq.rogue.MongoHelpers.MongoBuilder._
import io.fsq.rogue.{FindAndModifyQuery, ModifyQuery, Query}
import io.fsq.rogue.QueryHelpers._
import io.fsq.rogue.index.UntypedMongoIndex
import org.bson.{BsonDocument, BsonReader, BsonWriter}
import org.mongodb.scala.result.{
  DeleteResult,
  InsertManyResult,
  InsertOneResult,
  UpdateResult
}

import scala.collection.mutable.ListBuffer
import org.bson.conversions.Bson

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.ClassTag
import scala.util.Try
import org.bson.codecs.{
  Codec,
  DecoderContext,
  EncoderContext,
  RawBsonDocumentCodec
}
import org.bson.codecs.configuration.CodecRegistries
import org.reactivestreams.Publisher

import scala.reflect._
//rename to reactive
trait AsyncBsonDBCollectionFactory[MB] {

  def getDBCollection[M <: MB](query: Query[M, _, _])(implicit
      dba: MongoDatabase
  ): MongoCollection[BsonDocument]

  def getPrimaryDBCollection[M <: MB](query: Query[M, _, _])(implicit
      dba: MongoDatabase
  ): MongoCollection[BsonDocument]

  def getInstanceName[M <: MB](query: Query[M, _, _])(implicit
      dba: MongoDatabase
  ): String

  // A set of of indexes, which are ordered lists of field names
  def getIndexes[M <: MB](query: Query[M, _, _])(implicit
      dba: MongoDatabase
  ): Option[Seq[UntypedMongoIndex]]

}

class MongoAsyncBsonJavaDriverAdapter[MB](
    dbCollectionFactory: AsyncBsonDBCollectionFactory[MB]
) {

  private def getDBCollection[M <: MB, R](
      query: Query[M, _, _],
      readPreference: Option[ReadPreference]
  )(implicit dba: MongoDatabase): MongoCollection[BsonDocument] = {
    val c = dbCollectionFactory.getDBCollection(query)
    (readPreference.toSeq ++ query.readPreference.toSeq).headOption
      .fold(c)(c.withReadPreference)
  }

  def count[M <: MB](
      query: Query[M, _, _],
      readPreference: Option[ReadPreference]
  )(implicit dba: MongoDatabase): Future[Long] = {
    val queryClause = query
    val condition: Bson = buildCondition(queryClause.condition)
    //we don't care for skip, limit in count - maybe we deviate from original, but it makes no sense anyways
    val coll = getDBCollection(query, readPreference)
    //val callback = new PromiseLongCallbackBridge()
    val obs: SingleObservable[Long] =
      if (queryClause.lim.isDefined || queryClause.sk.isDefined) {
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

  def estimatedDocumentCount[M <: MB](
      query: Query[M, _, _],
      readPreference: Option[ReadPreference]
  )(implicit dba: MongoDatabase): Future[Long] = {
    val coll = getDBCollection(query, readPreference)
    val obs: SingleObservable[Long] = coll.estimatedDocumentCount()
    obs.toFuture()
  }

  def exists[M <: MB](
      query: Query[M, _, _],
      readPreference: Option[ReadPreference]
  )(implicit dba: MongoDatabase): Future[Boolean] = {
    val queryClause = query
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
    obs.subscribe(
      cnt => p.success(cnt > 0L),
      err => p.failure(err),
      () => p.trySuccess(false)
    )
    p.future
  }

  def countDistinct[M <: MB, R: ClassTag](
      query: Query[M, _, _],
      key: String,
      readPreference: Option[ReadPreference]
  )(implicit dba: MongoDatabase): Future[Long] = {
    val queryClause = query
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val p = Promise[Long]()
    val arr = new ListBuffer[R]
    coll
      .distinct[R](key, cnd)
      .subscribe(
        r => arr += (r),
        err => p.failure(err),
        () => p.trySuccess(arr.length)
      )
    p.future
  }

  def distinct[M <: MB, R: ClassTag](
      query: Query[M, _, _],
      key: String,
      readPreference: Option[ReadPreference]
  )(implicit dba: MongoDatabase): Future[Seq[R]] = {
    val queryClause = query
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    coll.distinct[R](key, cnd).toFuture()
  }

  def find[M <: MB, R](
      query: Query[M, _, _],
      serializer: RogueBsonRead[R],
      readPreference: Option[ReadPreference] = None
  )(implicit dba: MongoDatabase): Future[Seq[R]] = {
    val queryClause = query
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val sel: Bson = queryClause.select
      .map(buildSelect)
      .getOrElse(BasicDBObjectBuilder.start.get.asInstanceOf[BasicDBObject])
    val ord = queryClause.order.map(buildOrder)

    //sort, hints
    val cursor = coll.find(cnd).projection(sel)
    queryClause.lim.foreach(cursor.limit _)
    queryClause.sk.foreach(cursor.skip _)
    ord.foreach(cursor.sort _)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    cursor.map(serializer.fromDocument).toFuture()
  }

  private[this] val rawDocumentCodec = new RawBsonDocumentCodec

  def findPublisher[M <: MB, R: ClassTag](
      query: Query[M, _, _],
      readSerializer: RogueBsonRead[R],
      batchSize: Int,
      readPreference: Option[ReadPreference] = None
  )(implicit dba: MongoDatabase): Publisher[R] = {
    val classR = classTag[R].runtimeClass.asInstanceOf[Class[R]]
    val queryClause = query

    val cnd: Bson = buildCondition(queryClause.condition)
    val sel: Bson = queryClause.select
      .map(buildSelect)
      .getOrElse(BasicDBObjectBuilder.start.get.asInstanceOf[BasicDBObject])
    val ord = queryClause.order.map(buildOrder)
    val baseColl = getDBCollection(query, readPreference)

    val encoder = Try {
      baseColl.codecRegistry.get(classR)
    }

    val rCodec = new Codec[R] {

      override def decode(
          reader: BsonReader,
          decoderContext: DecoderContext
      ): R = {
        val bsonDoc = rawDocumentCodec.decode(reader, decoderContext)
        readSerializer.fromDocument(bsonDoc)
      }

      //don't care - only read
      override def encode(
          writer: BsonWriter,
          value: R,
          encoderContext: EncoderContext
      ): Unit = {
        encoder
          .map(_.encode(writer, value, encoderContext))
          .getOrElse(
            throw new IllegalStateException(
              s"Encoder not implemented encoder for ${classR} - only reader in fetch - query was ${cnd} / ${sel}"
            )
          )
      }

      override def getEncoderClass: Class[R] = classR.asInstanceOf[Class[R]]
    }
    val singleR = CodecRegistries.fromCodecs(rCodec)
    val cr = CodecRegistries.fromRegistries(singleR, baseColl.codecRegistry)

    val coll = baseColl.withCodecRegistry(cr)
    //check if serializer will work - quite possible that no, and separate mapper from Document-> R will be needed
    //sort, hints

    val cursor = coll.find[R](cnd).projection(sel)
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
      readPreference: Option[ReadPreference] = None
  )(implicit dba: MongoDatabase): Future[Option[R]] = {
    val queryClause = query
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val sel: Bson = queryClause.select
      .map(buildSelect)
      .getOrElse(BasicDBObjectBuilder.start.get.asInstanceOf[BasicDBObject])
    val ord = queryClause.order.map(buildOrder)
    val cursor = coll.find(cnd).projection(sel)
    queryClause.lim.foreach(cursor.limit _)
    queryClause.sk.foreach(cursor.skip _)
    ord.foreach(cursor.sort _)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    cursor.map(serializer.fromDocument).headOption()
  }

  def foreach[M <: MB, R](
      query: Query[M, _, _],
      f: BsonDocument => Unit,
      readPreference: Option[ReadPreference] = None
  )(implicit dba: MongoDatabase): Future[Unit] = {
    val queryClause = query
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val cursor = coll.find(cnd)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    val p = Promise[Unit]()
    cursor.subscribe(f, (t: Throwable) => p.failure(t), () => p.success(()))
    p.future
  }

  def delete[M <: MB](query: Query[M, _, _], writeConcern: WriteConcern)(
      implicit dba: MongoDatabase
  ): Future[DeleteResult] = {
    val queryClause = query
    val cnd = buildCondition(queryClause.condition)
    val coll = dbCollectionFactory.getPrimaryDBCollection(query)
    coll.deleteMany(cnd).toFuture()
  }

  private[this] def modifyInternal[M <: MB, RT](
      mod: ModifyQuery[M, _],
      upsert: Boolean,
      multi: Boolean,
      writeConcern: WriteConcern
  )(implicit dba: MongoDatabase): Future[UpdateResult] = {
    val modClause = mod
    if (!modClause.mod.clauses.isEmpty) {
      val q: Bson = buildCondition(modClause.query.condition)
      val m: Bson = buildModify(modClause.mod)
      val coll = dbCollectionFactory
        .getPrimaryDBCollection(modClause.query)
        .withWriteConcern(writeConcern)
      val updateOptions = new UpdateOptions().upsert(upsert)

      val updater = if (multi) {
        coll.updateMany(q, m, updateOptions)
      } else {
        coll.updateOne(q, m, updateOptions)
      }
      updater.toFuture()

    } else {
      Future.successful(UpdateResult.unacknowledged())
    }
  }

  def modify[M <: MB](
      mod: ModifyQuery[M, _],
      upsert: Boolean,
      multi: Boolean,
      writeConcern: WriteConcern
  )(implicit dba: MongoDatabase): Future[UpdateResult] = {
    modifyInternal[M, Unit](mod, upsert, multi, writeConcern)
  }

  /** @param mod
    *   @param upsert
    * @param multi
    *   @param writeConcern
    * @param dba
    *   @tparam M
    * @return
    *   UpdateResult if writeConcern >= Acknowledged, so writer can learn about
    *   relevant data (number of updated, created etc)
    */
  def modifyRet[M <: MB](
      mod: ModifyQuery[M, _],
      upsert: Boolean,
      multi: Boolean,
      writeConcern: WriteConcern
  )(implicit dba: MongoDatabase): Future[UpdateResult] = {
    modifyInternal[M, UpdateResult](mod, upsert, multi, writeConcern)
  }

  def findAndModify[M <: MB, R](
      mod: FindAndModifyQuery[M, R],
      returnNew: Boolean,
      upsert: Boolean,
      remove: Boolean,
      writeConcern: WriteConcern
  )(
      f: BsonDocument => Option[R]
  )(implicit dba: MongoDatabase): Future[Option[R]] = {
    val modClause = mod
    if (!modClause.mod.clauses.isEmpty || remove) {
      val query = modClause.query
      val cnd: Bson = buildCondition(query.condition)
      val ord = query.order.map(buildOrder)
      val m: Bson = buildModify(modClause.mod)
      val coll = dbCollectionFactory
        .getPrimaryDBCollection(query)
        .withWriteConcern(writeConcern)
      val retDoc =
        if (returnNew) ReturnDocument.AFTER else ReturnDocument.BEFORE
      val updater = if (remove) {
        val opts = new FindOneAndDeleteOptions()
        ord.map(dbo => opts.sort(dbo))
        coll.findOneAndDelete(cnd, opts)
      } else {
        val opts =
          new FindOneAndUpdateOptions().returnDocument(retDoc).upsert(upsert)
        ord.map(dbo => opts.sort(dbo))
        coll.findOneAndUpdate(cnd, m, opts)
      }
      val p = Promise[Option[R]]()
      updater
        .map(f)
        .subscribe(
          r => p.success(r),
          err => p.failure(err),
          () => p.trySuccess(None)
        )
      p.future
    } else Future.successful(None)
  }

  def insertOne[M <: MB, R](
      query: Query[M, R, _],
      doc: BsonDocument,
      writeConcern: WriteConcern
  )(implicit dba: MongoDatabase): Future[InsertOneResult] = {
    val collection = dbCollectionFactory
      .getPrimaryDBCollection(query)
      .withWriteConcern(writeConcern)
    collection.insertOne(doc).toFuture()
  }

  def insertMany[M <: MB, R](
      query: Query[M, R, _],
      docs: Seq[BsonDocument],
      writeConcern: WriteConcern
  )(implicit dba: MongoDatabase): Future[InsertManyResult] = {
    val collection = dbCollectionFactory
      .getPrimaryDBCollection(query)
      .withWriteConcern(writeConcern)
    collection.insertMany(docs).toFuture()
  }

  def replaceOne[M <: MB, R](
      query: Query[M, R, _],
      doc: BsonDocument,
      upsert: Boolean,
      writeConcern: WriteConcern
  )(implicit dba: MongoDatabase): Future[UpdateResult] = {
    val collection = dbCollectionFactory
      .getPrimaryDBCollection(query)
      .withWriteConcern(writeConcern)
    val filter = Document(("_id", doc.get("_id")))
    collection
      .replaceOne(filter, doc, new ReplaceOptions().upsert(upsert))
      .toFuture()
  }

  def batch[M <: MB, R: ClassTag, T](
      query: Query[M, R, _],
      serializer: RogueBsonRead[R],
      f: Seq[R] => Future[Seq[T]],
      batchSize: Int,
      readPreference: Option[ReadPreference]
  )(implicit dba: MongoDatabase, ec: ExecutionContext): Future[Seq[T]] = {
    val publisher = findPublisher(query, serializer, batchSize, readPreference)
    val batchSubscriber = new BatchingSubscriber(batchSize, f)
    publisher.subscribe(batchSubscriber)
    batchSubscriber.toFuture()
  }

}
