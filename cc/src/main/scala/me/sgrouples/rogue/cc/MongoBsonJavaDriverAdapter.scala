package me.sgrouples.rogue.cc

import com.mongodb._
import io.fsq.rogue.Iter._
import io.fsq.rogue.index.UntypedMongoIndex

import com.mongodb.client._
import com.mongodb.client.model._
import io.fsq.rogue.{ FindAndModifyQuery, ModifyQuery, Query, RogueException }
import org.bson.BsonDocument
import org.bson.conversions.Bson

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import scala.util.{ Failure, Success, Try }

trait BsonDBCollectionFactory[MB] {
  def getDBCollection[M <: MB](query: Query[M, _, _])(implicit db: MongoDatabase): MongoCollection[BsonDocument]

  def getPrimaryDBCollection[M <: MB](query: Query[M, _, _])(implicit db: MongoDatabase): MongoCollection[BsonDocument]

  //def getPrimaryDBCollection(record: RB): MongoCollection[BsonDocument]

  def getInstanceName[M <: MB](query: Query[M, _, _])(implicit db: MongoDatabase): String

  //def getInstanceName(record: RB): String

  // A set of of indexes, which are ordered lists of field names
  def getIndexes[M <: MB](query: Query[M, _, _])(implicit db: MongoDatabase): Option[Seq[UntypedMongoIndex]]

}

class MongoBsonJavaDriverAdapter[MB](
  dbCollectionFactory: BsonDBCollectionFactory[MB],
  decoderFactoryFunc: (MB) => DBDecoderFactory = (m: MB) => DefaultDBDecoder.FACTORY) {

  import io.fsq.rogue.MongoHelpers.MongoBuilder._
  import io.fsq.rogue.QueryHelpers._

  private[rogue] def runCommand[M <: MB, T](
    descriptionFunc: () => String,
    query: Query[M, _, _])(f: => T)(implicit db: MongoDatabase): T = {
    // Use nanoTime instead of currentTimeMillis to time the query since
    // currentTimeMillis only has 10ms granularity on many systems.
    val start = System.nanoTime
    val instanceName: String = dbCollectionFactory.getInstanceName(query)
    // Note that it's expensive to call descriptionFunc, it does toString on the Query
    // the logger methods are call by name
    try {
      logger.onExecuteQuery(query, instanceName, descriptionFunc(), f)
    } catch {
      case e: Exception =>
        throw new RogueException("Mongo query on %s [%s] failed after %d ms".
          format(instanceName, descriptionFunc(),
            (System.nanoTime - start) / (1000 * 1000)), e)
    } finally {
      logger.log(query, instanceName, descriptionFunc(), (System.nanoTime - start) / (1000 * 1000))
    }
  }

  private def getDBCollection[M <: MB, R](
    query: Query[M, _, _],
    readPreference: Option[ReadPreference])(implicit db: MongoDatabase): MongoCollection[BsonDocument] = {
    val c = dbCollectionFactory.getDBCollection(query)
    (readPreference.toSeq ++ query.readPreference.toSeq).headOption.fold(c)(c.withReadPreference)
  }

  def count[M <: MB](query: Query[M, _, _], readPreference: Option[ReadPreference])(implicit db: MongoDatabase): Long = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val condition: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    if (queryClause.lim.isDefined || queryClause.sk.isDefined) {
      val options = new CountOptions()
      queryClause.lim.map(options.limit(_))
      queryClause.sk.map(options.skip(_))
      coll.count(condition, options)
    } else {
      coll.count(condition)
    }
  }

  def countDistinct[M <: MB, R](
    query: Query[M, _, _],
    key: String,
    ct: ClassTag[R],
    readPreference: Option[ReadPreference])(implicit db: MongoDatabase): Long = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val rClass = ct.runtimeClass.asInstanceOf[Class[R]]
    coll.distinct(key, cnd, rClass).asScala.toSet.size.toLong
  }

  def distinct[M <: MB, R](
    query: Query[M, _, _],
    key: String,
    ct: ClassTag[R],
    readPreference: Option[ReadPreference])(implicit db: MongoDatabase): Iterable[R] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)
    val rClass = ct.runtimeClass.asInstanceOf[Class[R]]
    coll.distinct[R](key, cnd, rClass).asScala
  }

  def delete[M <: MB](
    query: Query[M, _, _],
    writeConcern: WriteConcern)(implicit db: MongoDatabase): Unit = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = dbCollectionFactory.getPrimaryDBCollection(query)
    coll.deleteMany(cnd)
  }

  def insertOne[M <: MB, R](query: Query[M, R, _], doc: BsonDocument, writeConcern: WriteConcern)(implicit db: MongoDatabase): Unit = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(query)
    collection.insertOne(doc)
  }

  def insertMany[M <: MB, R](query: Query[M, R, _], docs: Seq[BsonDocument], writeConcern: WriteConcern)(implicit db: MongoDatabase): Unit = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(query)
    collection.insertMany(docs.toList.asJava)
  }

  def modify[M <: MB](
    mod: ModifyQuery[M, _],
    upsert: Boolean,
    multi: Boolean,
    writeConcern: WriteConcern)(implicit db: MongoDatabase): Unit = {
    val modClause = transformer.transformModify(mod)
    validator.validateModify(modClause, dbCollectionFactory.getIndexes(modClause.query))
    if (!modClause.mod.clauses.isEmpty) {
      val q: Bson = buildCondition(modClause.query.condition)
      val m: Bson = buildModify(modClause.mod)
      val coll = dbCollectionFactory.getPrimaryDBCollection(modClause.query)
      val updateOptions = new UpdateOptions().upsert(upsert)
      if (multi) {
        coll.updateMany(q, m, updateOptions)
      } else {
        coll.updateOne(q, m, updateOptions)
      }
    }
  }

  def findAndModify[M <: MB, R](
    mod: FindAndModifyQuery[M, R],
    returnNew: Boolean,
    upsert: Boolean,
    remove: Boolean)(f: BsonDocument => R)(implicit db: MongoDatabase): Option[R] = {
    val modClause = transformer.transformFindAndModify(mod)
    validator.validateFindAndModify(modClause, dbCollectionFactory.getIndexes(modClause.query))
    if (!modClause.mod.clauses.isEmpty || remove) {
      val query = modClause.query
      val cnd: Bson = buildCondition(query.condition)
      val ord = query.order.map(buildOrder)
      val m: Bson = buildModify(modClause.mod)
      val coll = dbCollectionFactory.getPrimaryDBCollection(query)
      val retDoc = if (returnNew) ReturnDocument.AFTER else ReturnDocument.BEFORE
      val ret = if (remove) {
        val opts = new FindOneAndDeleteOptions()
        ord.map(dbo => opts.sort(dbo))
        coll.findOneAndDelete(cnd, opts)
      } else {
        val opts = new FindOneAndUpdateOptions().returnDocument(retDoc).upsert(upsert)
        ord.map(dbo => opts.sort(dbo))
        coll.findOneAndUpdate(cnd, m, opts)
      }
      Option(ret).map(f)
    } else {
      None
    }
  }

  def findIterable[M <: MB, R](query: Query[M, _, _], batchSize: Option[Int] = None, readPreference: Option[ReadPreference] = None)(implicit db: MongoDatabase): FindIterable[BsonDocument] = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd: Bson = buildCondition(queryClause.condition)
    val coll = getDBCollection(query, readPreference)

    val sel: Bson = queryClause.select.map(buildSelect)
      .getOrElse(BasicDBObjectBuilder
        .start.get.asInstanceOf[BasicDBObject])

    val ord = queryClause.order.map(buildOrder)
    val cursor = coll.find(cnd).projection(sel)

    batchSize.map(cursor.batchSize _)
    queryClause.lim.foreach(cursor.limit _)
    queryClause.sk.foreach(cursor.skip _)
    ord.foreach(cursor.sort _)
    query.hint.foreach(h => cursor.hint(buildHint(h)))
    cursor
  }

  def fineOne[M <: MB, R](query: Query[M, _, _], serializer: RogueBsonRead[R])(implicit db: MongoDatabase): Option[R] = {
    Option(findIterable(query).first()).flatMap(serializer.fromDocumentOpt)
  }

  def find[M <: MB, R](query: Query[M, _, _], serializer: RogueBsonRead[R], readPreference: Option[ReadPreference])(implicit db: MongoDatabase): Seq[R] = {
    val mb = Seq.newBuilder[R]

    val it = findIterable(query, None, readPreference).iterator()
    while (it.hasNext) {
      mb += serializer.fromDocument(it.next())
    }
    mb.result()
  }

  def iterate[M <: MB, R, S](
    query: Query[M, R, _],
    initialState: S,
    f: BsonDocument => R,
    readPreference: Option[ReadPreference] = None)(handler: (S, Event[R]) => Command[S])(implicit db: MongoDatabase): S = {
    def getObject(cursor: MongoCursor[BsonDocument]): Try[R] = {
      Try(f(cursor.next()))
    }

    @scala.annotation.tailrec
    def iter(cursor: MongoCursor[BsonDocument], curState: S): S = {
      if (cursor.hasNext) {
        getObject(cursor) match {
          case Failure(e) => handler(curState, Error(e)).state
          case Success(r) => handler(curState, Item(r)) match {
            case Continue(s) => iter(cursor, s)
            case Return(s) => s
          }
        }
      } else {
        handler(curState, EOF).state
      }
    }

    val cursor: MongoCursor[BsonDocument] = findIterable(query, None, readPreference).iterator()
    iter(cursor, initialState)
  }

  def iterateBatch[M <: MB, R, S](
    query: Query[M, R, _],
    batchSize: Int,
    initialState: S,
    f: BsonDocument => R,
    readPreference: Option[ReadPreference] = None)(handler: (S, Event[Seq[R]]) => Command[S])(implicit db: MongoDatabase): S = {
    val buf = new ListBuffer[R]

    def getBatch(cursor: MongoCursor[BsonDocument]): Try[Seq[R]] = {
      Try {
        buf.clear()
        // ListBuffer#length is O(1) vs ListBuffer#size is O(N) (true in 2.9.x, fixed in 2.10.x)
        while (cursor.hasNext && buf.length < batchSize) {
          buf += f(cursor.next)
        }
        buf.toList
      }
    }

    @scala.annotation.tailrec
    def iter(cursor: MongoCursor[BsonDocument], curState: S): S = {
      if (cursor.hasNext) {
        getBatch(cursor) match {
          case Failure(e) => handler(curState, Error(e)).state
          case Success(Nil) => handler(curState, EOF).state
          case Success(rs) => handler(curState, Item(rs)) match {
            case Continue(s) => iter(cursor, s)
            case Return(s) => s
          }
        }
      } else {
        handler(curState, EOF).state
      }
    }
    val cursor = findIterable(query, Some(batchSize), readPreference).iterator()
    iter(cursor, initialState)
  }

  def replaceOne[M <: MB, R](query: Query[M, R, _], doc: BsonDocument, upsert: Boolean, writeConcern: WriteConcern)(implicit db: MongoDatabase): Unit = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(query)
    val filter = new BsonDocument("_id", doc.get("_id"))
    collection.replaceOne(filter, doc, new UpdateOptions().upsert(upsert))
  }

}
