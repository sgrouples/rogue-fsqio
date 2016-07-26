package me.sgrouples.rogue.cc

import com.mongodb.client.MongoCollection
import io.fsq.rogue.index.{ IndexedRecord, UntypedMongoIndex }
import io.fsq.rogue.{ Query, QueryHelpers }
import org.bson.BsonDocument

object CcDBCollectionFactory extends BsonDBCollectionFactory[CcMeta[_]] {
  type TCM = CcMeta[_]
  val bsonDocClass = classOf[BsonDocument]

  //temorary codec registry until all needed machinery converted from BasicDBObject to BsonDocument
  //[M <: MongoRecord[_] with MongoMetaRecord[_]
  override def getDBCollection[M <: TCM](query: Query[M, _, _]): MongoCollection[BsonDocument] = {
    query.meta.dbs().getCollection(query.collectionName, bsonDocClass)
  }

  override def getPrimaryDBCollection[M <: TCM](query: Query[M, _, _]): MongoCollection[BsonDocument] = {
    getDBCollection(query)
  }

  protected def getPrimaryDBCollection(meta: CcMeta[_], collectionName: String): MongoCollection[BsonDocument] = {
    meta.dbs().getCollection(collectionName, bsonDocClass)
  }

  /*override def getPrimaryDBCollection(record: MongoRecord[_]): MongoCollection[BsonDocument] = {
    getPrimaryDBCollection(record.meta, record.meta.collectionName)
  }*/

  override def getInstanceName[M <: TCM](query: Query[M, _, _]): String = {
    query.meta.dbs().getName
  }

  /*override def getInstanceName(record: MongoRecord[_]): String =
    record.meta.connectionIdentifier.toString
*/
  /**
   * Retrieves the list of indexes declared for the record type associated with a
   * query. If the record type doesn't declare any indexes, then returns None.
   *
   * @param query the query
   * @return the list of indexes, or an empty list.
   */
  override def getIndexes[M <: TCM](query: Query[M, _, _]): Option[Seq[UntypedMongoIndex]] = {
    val queryMetaRecord = query.meta
    if (queryMetaRecord.isInstanceOf[IndexedRecord[_]]) {
      Some(queryMetaRecord.asInstanceOf[IndexedRecord[_]].mongoIndexList)
    } else {
      None
    }
  }
}

class CcAdapter(dbCollectionFactory: BsonDBCollectionFactory[CcMeta[_]]) extends MongoBsonJavaDriverAdapter(dbCollectionFactory)

object CcAdapter extends CcAdapter(CcDBCollectionFactory)

class CcQueryExecutor(override val adapter: MongoBsonJavaDriverAdapter[CcMeta[_]]) extends BsonQueryExecutor[CcMeta[_]] with BsonReadWriteSerializers[CcMeta[_]] {
  override def defaultWriteConcern = QueryHelpers.config.defaultWriteConcern
}

object CcQueryExecutor extends CcQueryExecutor(CcAdapter)