package io.fsq.rogue.lift

import io.fsq.rogue.index.{ IndexedRecord, UntypedMongoIndex }
import io.fsq.rogue.MongoHelpers.MongoSelect
import com.mongodb.DBObject
import com.mongodb.async.client.MongoCollection
import net.liftweb.mongodb.record.{ MongoMetaRecord, MongoRecord }
import net.liftweb.mongodb.MongoAsync
import org.bson.Document
import io.fsq.rogue._

object LiftAsyncDBCollectionFactory extends AsyncDBCollectionFactory[MongoRecord[_] with MongoMetaRecord[_], MongoRecord[_]] {
  override def getDBCollection[M <: MongoRecord[_] with MongoMetaRecord[_]](query: Query[M, _, _]): MongoCollection[Document] = {
    MongoAsync.use(query.meta.connectionIdentifier) { db =>
      db.getCollection(query.collectionName)
    }
  }

  override def getPrimaryDBCollection[M <: MongoRecord[_] with MongoMetaRecord[_]](query: Query[M, _, _]): MongoCollection[Document] = {
    MongoAsync.use(query.meta /* TODO: .master*/ .connectionIdentifier) { db =>
      db.getCollection(query.collectionName)
    }
  }

  protected def getPrimaryDBCollection(meta: MongoMetaRecord[_], collectionName: String): MongoCollection[Document] = {
    MongoAsync.use(meta /* TODO: .master*/ .connectionIdentifier) { db =>
      db.getCollection(collectionName)
    }
  }

  override def getPrimaryDBCollection(record: MongoRecord[_]): MongoCollection[Document] = {
    getPrimaryDBCollection(record.meta, record.meta.collectionName)
  }

  override def getInstanceName[M <: MongoRecord[_] with MongoMetaRecord[_]](query: Query[M, _, _]): String = {
    query.meta.connectionIdentifier.toString
  }

  override def getInstanceName(record: MongoRecord[_]): String =
    record.meta.connectionIdentifier.toString

  /**
   * Retrieves the list of indexes declared for the record type associated with a
   * query. If the record type doesn't declare any indexes, then returns None.
   *
   * @param query the query
   * @return the list of indexes, or an empty list.
   */
  override def getIndexes[M <: MongoRecord[_] with MongoMetaRecord[_]](query: Query[M, _, _]): Option[Seq[UntypedMongoIndex]] = {
    val queryMetaRecord = query.meta
    if (queryMetaRecord.isInstanceOf[IndexedRecord[_]]) {
      Some(queryMetaRecord.asInstanceOf[IndexedRecord[_]].mongoIndexList)
    } else {
      None
    }
  }

}

class LiftAsyncAdapter(dbCollectionFactory: AsyncDBCollectionFactory[MongoRecord[_] with MongoMetaRecord[_], MongoRecord[_]]) extends MongoAsyncJavaDriverAdapter(dbCollectionFactory)

object LiftAsyncAdapter extends LiftAsyncAdapter(LiftAsyncDBCollectionFactory)

class LiftAsyncQueryExecutor(override val adapter: MongoAsyncJavaDriverAdapter[MongoRecord[_] with MongoMetaRecord[_], MongoRecord[_]]) extends AsyncQueryExecutor[MongoRecord[_] with MongoMetaRecord[_], MongoRecord[_]] {
  override def defaultWriteConcern = QueryHelpers.config.defaultWriteConcern

  override lazy val optimizer = new QueryOptimizer

  override protected def readSerializer[M <: MongoRecord[_] with MongoMetaRecord[_], R](
    meta: M,
    select: Option[MongoSelect[M, R]]): RogueReadSerializer[R] = {
    new RogueReadSerializer[R] {
      override def fromDBObject(dbo: DBObject): R = select match {
        case Some(MongoSelect(fields, transformer, true, _)) if fields.isEmpty =>
          // A MongoSelect clause exists, but has empty fields. Return null.
          // This is used for .exists(), where we just want to check the number
          // of returned results is > 0.
          transformer(null)
        case Some(MongoSelect(fields, transformer, _, _)) =>
          val inst = meta.createRecord.asInstanceOf[MongoRecord[_]]

          LiftQueryExecutorHelpers.setInstanceFieldFromDbo(inst, dbo, "_id")

          val values =
            fields.map(fld => {
              val valueOpt = LiftQueryExecutorHelpers.setInstanceFieldFromDbo(inst, dbo, fld.field.name)
              fld.valueOrDefault(valueOpt)
            })

          transformer(values)
        case None =>
          meta.fromDBObject(dbo).asInstanceOf[R]
      }

      override def fromDocument(dbo: Document): R = select match {
        case Some(MongoSelect(fields, transformer, true, _)) if fields.isEmpty =>
          // A MongoSelect clause exists, but has empty fields. Return null.
          // This is used for .exists(), where we just want to check the number
          // of returned results is > 0.
          transformer(null)
        case Some(MongoSelect(fields, transformer, _, _)) =>
          val inst = meta.createRecord.asInstanceOf[MongoRecord[_]]

          LiftQueryExecutorHelpers.setInstanceFieldFromDoc(inst, dbo, "_id")

          val values =
            fields.map(fld => {
              val valueOpt = LiftQueryExecutorHelpers.setInstanceFieldFromDoc(inst, dbo, fld.field.name)
              fld.valueOrDefault(valueOpt)
            })

          transformer(values)
        case None =>
          meta.fromDocument(dbo).asInstanceOf[R]
      }
    }
  }

  override protected def writeSerializer(record: MongoRecord[_]): RogueWriteSerializer[MongoRecord[_]] = {
    new RogueWriteSerializer[MongoRecord[_]] {
      override def toDBObject(record: MongoRecord[_]): DBObject = {
        record.asDBObject
      }

      override def toDocument(record: MongoRecord[_]): Document = {
        record.asDocument
      }
    }
  }
}

object LiftAsyncQueryExecutor extends LiftAsyncQueryExecutor(LiftAsyncAdapter)
