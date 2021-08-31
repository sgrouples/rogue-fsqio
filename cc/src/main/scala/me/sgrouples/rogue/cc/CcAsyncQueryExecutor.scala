package me.sgrouples.rogue.cc

import io.fsq.rogue.index.{IndexedRecord, UntypedMongoIndex}
import org.mongodb.scala._
import io.fsq.rogue._
import org.mongodb.scala.bson.BsonDocument

object CcAsyncDBCollectionFactory
    extends AsyncBsonDBCollectionFactory[CcMeta[_]] {
  type TCM = CcMeta[_]
  //temporary codec registry until all needed machinery converted from BasicDBObject to BsonDocument
  //[M <: MongoRecord[_] with MongoMetaRecord[_]
  override def getDBCollection[M <: TCM](
      query: Query[M, _, _]
  )(implicit dba: MongoDatabase): MongoCollection[BsonDocument] = {
    dba.getCollection[BsonDocument](query.collectionName)
  }

  override def getPrimaryDBCollection[M <: TCM](
      query: Query[M, _, _]
  )(implicit dba: MongoDatabase): MongoCollection[BsonDocument] = {
    getDBCollection(query)
  }

  protected def getPrimaryDBCollection(meta: CcMeta[_], collectionName: String)(
      implicit dba: MongoDatabase
  ): MongoCollection[BsonDocument] = {
    dba
      .getCollection[BsonDocument](collectionName)
      .withReadPreference(ReadPreference.primary())
  }

  override def getInstanceName[M <: TCM](
      query: Query[M, _, _]
  )(implicit dba: MongoDatabase): String = {
    dba.name
  }

  /** Retrieves the list of indexes declared for the record type associated with
    * a query. If the record type doesn't declare any indexes, then returns
    * None.
    *
    * @param query
    *   the query
    * @return
    *   the list of indexes, or an empty list.
    */
  override def getIndexes[M <: TCM](
      query: Query[M, _, _]
  )(implicit dba: MongoDatabase): Option[Seq[UntypedMongoIndex]] = {
    val queryMetaRecord = query.meta
    if (queryMetaRecord.isInstanceOf[IndexedRecord[_]]) {
      Some(queryMetaRecord.asInstanceOf[IndexedRecord[_]].mongoIndexList)
    } else {
      None
    }
  }
}

class CcAsyncAdapter(
    dbCollectionFactory: AsyncBsonDBCollectionFactory[CcMeta[_]]
) extends MongoAsyncBsonJavaDriverAdapter(dbCollectionFactory)

object CcAsyncAdapter extends CcAsyncAdapter(CcAsyncDBCollectionFactory)

class CcAsyncQueryExecutor(
    override val adapter: MongoAsyncBsonJavaDriverAdapter[CcMeta[_]]
) extends AsyncBsonQueryExecutor[CcMeta[_]]
    with BsonReadWriteSerializers[CcMeta[_]] {
  override def defaultWriteConcern = QueryHelpers.config.defaultWriteConcern

}

object CcAsyncQueryExecutor extends CcAsyncQueryExecutor(CcAsyncAdapter)
