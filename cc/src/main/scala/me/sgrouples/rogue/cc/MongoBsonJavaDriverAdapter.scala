package me.sgrouples.rogue.cc

import com.mongodb.{BasicDBObjectBuilder, DBCursor, DBDecoderFactory, DBObject, DefaultDBDecoder, ReadPreference, WriteConcern}
import io.fsq.rogue.Iter._
import io.fsq.rogue.Rogue._
import io.fsq.rogue.index.UntypedMongoIndex
import java.util.concurrent.TimeUnit

import com.mongodb.client.{FindIterable, MongoCollection}
import com.mongodb.client.model.{CountOptions, UpdateOptions}
import io.fsq.rogue.{FindAndModifyQuery, ModifyQuery, Query, RogueException}
import org.bson.{BSONObject, BsonDocument}
import org.bson.conversions.Bson

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

trait BsonDBCollectionFactory[MB] {
  def getDBCollection[M <: MB](query: Query[M, _, _]): MongoCollection[BsonDocument]

  def getPrimaryDBCollection[M <: MB](query: Query[M, _, _]): MongoCollection[BsonDocument]

  //def getPrimaryDBCollection(record: RB): MongoCollection[BsonDocument]

  def getInstanceName[M <: MB](query: Query[M, _, _]): String

  //def getInstanceName(record: RB): String

  // A set of of indexes, which are ordered lists of field names
  def getIndexes[M <: MB](query: Query[M, _, _]): Option[Seq[UntypedMongoIndex]]

}

class MongoBsonJavaDriverAdapter[MB](
                                      dbCollectionFactory: BsonDBCollectionFactory[MB],
                                      decoderFactoryFunc: (MB) => DBDecoderFactory = (m: MB) => DefaultDBDecoder.FACTORY
                                    ) {

  import io.fsq.rogue.MongoHelpers.MongoBuilder._
  import io.fsq.rogue.QueryHelpers._

  private[rogue] def runCommand[M <: MB, T](descriptionFunc: () => String,
                                            query: Query[M, _, _])(f: => T): T = {
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

  def count[M <: MB](query: Query[M, _, _], readPreference: Option[ReadPreference]): Long = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val condition: DBObject = buildCondition(queryClause.condition)
    val descriptionFunc: () => String = () => buildConditionString("count", query.collectionName, queryClause)


    runCommand(descriptionFunc, queryClause) {
      val coll = dbCollectionFactory.getDBCollection(query)
      val condition: Bson = buildCondition(queryClause.condition)
      if (queryClause.lim.isDefined || queryClause.sk.isDefined) {
        val options = new CountOptions()
        queryClause.lim.map(options.limit(_))
        queryClause.sk.map(options.skip(_))
        coll.count(condition, options)
      } else {
        coll.count(condition)
      }
    }
  }

  def countDistinct[M <: MB](query: Query[M, _, _],
                             key: String,
                             readPreference: Option[ReadPreference]): Long = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)

    // TODO: fix this so it looks like the correct mongo shell command
    val descriptionFunc: () => String = () => buildConditionString("distinct", query.collectionName, queryClause)

    /*runCommand(descriptionFunc, queryClause) {
      val coll = dbCollectionFactory.getDBCollection(query)
      //coll.distinct(key, cnd, readPreference.getOrElse(coll.getReadPreference)).size()
    }*/
    -1L
  }

  def distinct[M <: MB, R](query: Query[M, _, _],
                           key: String,
                           ct: ClassTag[R],
                           readPreference: Option[ReadPreference])
                          (f: R => Unit): Unit = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)

    // TODO: fix this so it looks like the correct mongo shell command
    val descriptionFunc: () => String = () => buildConditionString("distinct", query.collectionName, queryClause)


    /*
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

    runCommand(descriptionFunc, queryClause) {
      val coll = dbCollectionFactory.getDBCollection(query)
      val rj = coll.distinct(key, cnd, readPreference.getOrElse(coll.getReadPreference))
      for (i <- 0 until rj.size) {
        f(rj.get(i).asInstanceOf[R])
      }
    }*/
    ()
  }

  def delete[M <: MB](query: Query[M, _, _],
                      writeConcern: WriteConcern): Unit = {
    val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)
    val descriptionFunc: () => String = () => buildConditionString("remove", query.collectionName, queryClause)

    runCommand(descriptionFunc, queryClause) {
      val coll = dbCollectionFactory.getPrimaryDBCollection(query)
      coll.deleteMany(cnd)
      //coll.remove(cnd, writeConcern)
    }
  }
  def insert[R](record: R, writeConcern: WriteConcern): Unit = {
    ???
    /*val collection = dbCollectionFactory.getPrimaryDBCollection(m)
    //dbCollectionFactory.getPrimaryDBCollection(record)
    val collectionName = collection.getName
    val instanceName = dbCollectionFactory.getInstanceName(record)
    logger.onExecuteWriteCommand("insert", collectionName, instanceName, dbo.toString, collection.insert(dbo, writeConcern))
    */
  }

  /*
  def insertAll(record: RB, dbos: Seq[DBObject], writeConcern: WriteConcern): Unit = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(record)
    val collectionName = collection.getName
    val instanceName = dbCollectionFactory.getInstanceName(record)
    val descriptionFunc = () => dbos.map(_.toString).mkString("[", ",", "]")
    logger.onExecuteWriteCommand("insert", collectionName, instanceName, descriptionFunc(), collection.insert(QueryHelpers.list(dbos), writeConcern))
  }

  def remove(record: RB, dbo: DBObject, writeConcern: WriteConcern): Unit = {
    val collection = dbCollectionFactory.getPrimaryDBCollection(record)
    val collectionName = collection.getName
    val instanceName = dbCollectionFactory.getInstanceName(record)
    logger.onExecuteWriteCommand("remove", collectionName, instanceName, dbo.toString, collection.remove(dbo, writeConcern))
  }
*/
  def modify[M <: MB](mod: ModifyQuery[M, _],
                      upsert: Boolean,
                      multi: Boolean,
                      writeConcern: WriteConcern): Unit = {
    val modClause = transformer.transformModify(mod)
    validator.validateModify(modClause, dbCollectionFactory.getIndexes(modClause.query))
    if (!modClause.mod.clauses.isEmpty) {
      val q = buildCondition(modClause.query.condition)
      val m = buildModify(modClause.mod)
      val descriptionFunc: () => String = {
        () => buildModifyString(mod.query.collectionName, modClause, upsert = upsert, multi = multi)
      }
      runCommand(descriptionFunc, modClause.query) {
        val coll = dbCollectionFactory.getPrimaryDBCollection(modClause.query)
        val opts = new UpdateOptions().upsert(upsert)
        if(multi)  coll.updateMany(q,m, opts)
        else coll.updateOne(q,m,opts)
      }
    }
  }

  def findAndModify[M <: MB, R](mod: FindAndModifyQuery[M, R],
                                returnNew: Boolean,
                                upsert: Boolean,
                                remove: Boolean)
                               (f: DBObject => R): Option[R] = {
    /*val modClause = transformer.transformFindAndModify(mod)
    validator.validateFindAndModify(modClause, dbCollectionFactory.getIndexes(modClause.query))
    if (!modClause.mod.clauses.isEmpty || remove) {
      val query = modClause.query
      val cnd = buildCondition(query.condition)
      val ord = query.order.map(buildOrder)
      val sel = query.select.map(buildSelect).getOrElse(BasicDBObjectBuilder.start.get)
      val m = buildModify(modClause.mod)
      val descriptionFunc: () => String = {
        () => buildFindAndModifyString(mod.query.collectionName, modClause, returnNew, upsert, remove)
      }

      ((runCommand(descriptionFunc, modClause.query) {
        val coll = dbCollectionFactory.getPrimaryDBCollection(query)
        val dbObj = coll.findAndModify(cnd, sel, ord.getOrElse(null), remove, m, returnNew, upsert)
        if (dbObj == null || dbObj.keySet.isEmpty) None
        else Option(dbObj).map(f)
      }
        None
    }
    else */ None
  }

  def query[M <: MB](query: Query[M, _, _],
                     batchSize: Option[Int],
                     readPreference: Option[ReadPreference])
                    (f: BsonDocument => Unit): Unit = {
    /*doQuery("find", query, batchSize, readPreference, setMaxTimeMS = true){cursor =>
      cursor.setDecoderFactory(decoderFactoryFunc(query.meta))
      while (cursor.hasNext)
        f(cursor.next)
    }*/
  }

  def iterate[M <: MB, R, S](query: Query[M, R, _],
                             initialState: S,
                             f: DBObject => R,
                             readPreference: Option[ReadPreference] = None)
                            (handler: (S, Event[R]) => Command[S]): S = {
    def getObject(cursor: DBCursor): Either[Exception, R] = {
      try {
        Right(f(cursor.next))
      } catch {
        case e: Exception => Left(e)
      }
    }

    @scala.annotation.tailrec
    def iter(cursor: DBCursor, curState: S): S = {
      if (cursor.hasNext) {
        getObject(cursor) match {
          case Left(e) => handler(curState, Error(e)).state
          case Right(r) => handler(curState, Item(r)) match {
            case Continue(s) => iter(cursor, s)
            case Return(s) => s
          }
        }
      } else {
        handler(curState, EOF).state
      }
    }

    /*doQuery("find", query, None, readPreference)( cursor => {
      cursor.setDecoderFactory(decoderFactoryFunc(query.meta))
      iter(cursor, initialState)
    })*/
    initialState
  }

  def iterateBatch[M <: MB, R, S](query: Query[M, R, _],
                                  batchSize: Int,
                                  initialState: S,
                                  f: DBObject => R,
                                  readPreference: Option[ReadPreference] = None)
                                 (handler: (S, Event[List[R]]) => Command[S]): S = {
    val buf = new ListBuffer[R]

    def getBatch(cursor: DBCursor): Either[Exception, List[R]] = {
      try {
        buf.clear()
        // ListBuffer#length is O(1) vs ListBuffer#size is O(N) (true in 2.9.x, fixed in 2.10.x)
        while (cursor.hasNext && buf.length < batchSize) {
          buf += f(cursor.next)
        }
        Right(buf.toList)
      } catch {
        case e: Exception => Left(e)
      }
    }

    @scala.annotation.tailrec
    def iter(cursor: DBCursor, curState: S): S = {
      if (cursor.hasNext) {
        getBatch(cursor) match {
          case Left(e) => handler(curState, Error(e)).state
          case Right(Nil) => handler(curState, EOF).state
          case Right(rs) => handler(curState, Item(rs)) match {
            case Continue(s) => iter(cursor, s)
            case Return(s) => s
          }
        }
      } else {
        handler(curState, EOF).state
      }
    }

    /*doQuery("find", query, Some(batchSize), readPreference)(cursor => {
      cursor.setDecoderFactory(decoderFactoryFunc(query.meta))
      iter(cursor, initialState)
    })*/
    initialState
  }


  def explain[M <: MB](query: Query[M, _, _]): String = {
    "explain"
    /*doQuery("find", query, None, None, setMaxTimeMS = true){cursor =>
      cursor.explain.toString
    }*/
  }

  private def doQuery[M <: MB, T](
                                   operation: String,
                                   query: Query[M, _, _],
                                   batchSize: Option[Int],
                                   readPreference: Option[ReadPreference],
                                   setMaxTimeMS: Boolean = false
                                 )(
                                   f: FindIterable[BSONObject] => T
                                 ): T = {
    /*val queryClause = transformer.transformQuery(query)
    validator.validateQuery(queryClause, dbCollectionFactory.getIndexes(queryClause))
    val cnd = buildCondition(queryClause.condition)
    val ord = queryClause.order.map(buildOrder)
    val sel = queryClause.select.map(buildSelect).getOrElse(BasicDBObjectBuilder.start.get)
    val hnt = queryClause.hint.map(buildHint)

    val descriptionFunc: () => String = () => buildQueryString(operation, query.collectionName, queryClause)

    runCommand(descriptionFunc, queryClause) {
      val coll = dbCollectionFactory.getDBCollection(query)
      val cursor = coll.find(cnd, sel)

      // Always apply batchSize *before* limit. If the caller passes a negative value to limit(),
      // the driver applies it instead to batchSize. (A negative batchSize means, return one batch
      // and close the cursor.) Then if we set batchSize, the negative "limit" is overwritten, and
      // the query executes without a limit.
      // http://api.mongodb.org/java/2.7.3/com/mongodb/DBCursor.html#limit(int)
      config.cursorBatchSize match {
        case None => {
          // Apply the batch size from the query
          batchSize.foreach(cursor.batchSize _)
        }
        case Some(None) => {
          // don't set batch size
        }
        case Some(Some(n)) => {
          // Use the configured default batch size
          cursor.batchSize(n)
        }
      }

      queryClause.lim.foreach(cursor.limit _)
      queryClause.sk.foreach(cursor.skip _)
      ord.foreach(cursor.sort _)
      //readPreference.orElse(queryClause.readPreference).foreach(cursor.setReadPreference _)
      //queryClause.maxScan.foreach(cursor addSpecial("$maxScan", _))
      //queryClause.comment.foreach(cursor addSpecial("$comment", _))
      //hnt.foreach(cursor hint _)
      if (setMaxTimeMS) {
        val configName = dbCollectionFactory.getDBCollection(query).getName
        config.maxTimeMSOpt(configName).foreach(maxTimeMS => cursor.maxTime(maxTimeMS, TimeUnit.MILLISECONDS))
      }
      /val ret = f(cursor)
      cursor.close()
      ret*/
      ???
    }
}
