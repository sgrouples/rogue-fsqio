package me.sgrouples.rogue.cc

import io.fsq.rogue.index.{ IndexedRecord, UntypedMongoIndex }
import io.fsq.rogue.MongoHelpers.MongoSelect
import com.mongodb.DBObject
import com.mongodb.async.client.{ MongoCollection, MongoDatabase }
import com.mongodb.reactivestreams.client.{ MongoClients => ReactiveMongoClients, MongoCollection => ReactiveMongoCollection }
import com.mongodb.reactivestreams.client.internal.{ MongoDatabaseImpl, ReactiveDbWrapper }
import org.bson.{ BsonArray, BsonDocument, BsonNull, BsonValue }
import io.fsq.rogue._
import org.bson.codecs.configuration.CodecRegistries

import scala.util.Try

object CcAsyncDBCollectionFactory extends AsyncBsonDBCollectionFactory[CcMeta[_]] {
  type TCM = CcMeta[_]
  val bsonDocClass = classOf[BsonDocument]
  //temorary codec registry until all needed machinery converted from BasicDBObject to BsonDocument
  //[M <: MongoRecord[_] with MongoMetaRecord[_]
  override def getDBCollection[M <: TCM](query: Query[M, _, _])(implicit dba: MongoDatabase): MongoCollection[BsonDocument] = {
    dba.getCollection(query.collectionName, bsonDocClass)
  }

  override def getReactiveCollection[M <: TCM](query: Query[M, _, _])(implicit dba: MongoDatabase): ReactiveMongoCollection[BsonDocument] = {
    val wrappedDb = new ReactiveDbWrapper(dba).wrap
    wrappedDb.getCollection(query.collectionName, bsonDocClass)
  }

  override def getPrimaryDBCollection[M <: TCM](query: Query[M, _, _])(implicit dba: MongoDatabase): MongoCollection[BsonDocument] = {
    getDBCollection(query)
  }

  protected def getPrimaryDBCollection(meta: CcMeta[_], collectionName: String)(implicit dba: MongoDatabase): MongoCollection[BsonDocument] = {
    dba.getCollection(collectionName, bsonDocClass)
  }

  /*override def getPrimaryDBCollection(record: MongoRecord[_]): MongoCollection[BsonDocument] = {
    getPrimaryDBCollection(record.meta, record.meta.collectionName)
  }*/

  override def getInstanceName[M <: TCM](query: Query[M, _, _])(implicit dba: MongoDatabase): String = {
    dba.getName
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
  override def getIndexes[M <: TCM](query: Query[M, _, _])(implicit dba: MongoDatabase): Option[Seq[UntypedMongoIndex]] = {
    val queryMetaRecord = query.meta
    if (queryMetaRecord.isInstanceOf[IndexedRecord[_]]) {
      Some(queryMetaRecord.asInstanceOf[IndexedRecord[_]].mongoIndexList)
    } else {
      None
    }
  }
}

class CcAsyncAdapter(dbCollectionFactory: AsyncBsonDBCollectionFactory[CcMeta[_]]) extends MongoAsyncBsonJavaDriverAdapter(dbCollectionFactory)

object CcAsyncAdapter extends CcAsyncAdapter(CcAsyncDBCollectionFactory)

class CcAsyncQueryExecutor(override val adapter: MongoAsyncBsonJavaDriverAdapter[CcMeta[_]]) extends AsyncBsonQueryExecutor[CcMeta[_]] with BsonReadWriteSerializers[CcMeta[_]] {
  override def defaultWriteConcern = QueryHelpers.config.defaultWriteConcern

}

object CcAsyncQueryExecutor extends CcAsyncQueryExecutor(CcAsyncAdapter)
