package me.sgrouples.rogue.cc

import io.fsq.rogue.index.{IndexedRecord, UntypedMongoIndex}
import io.fsq.rogue.MongoHelpers.MongoSelect
import com.mongodb.DBObject
import com.mongodb.async.client.MongoCollection
import org.bson.BsonDocument
import io.fsq.rogue._
import org.bson.codecs.configuration.CodecRegistries



object CcAsyncDBCollectionFactory extends AsyncBsonDBCollectionFactory[CcMeta[_]] {
  type TCM = CcMeta[_]
  val bsonDocClass = classOf[BsonDocument]
  //temorary codec registry until all needed machinery converted from BasicDBObject to BsonDocument
  val codecRegistry = CodecRegistries.fromRegistries(com.mongodb.MongoClient.getDefaultCodecRegistry())
  //[M <: MongoRecord[_] with MongoMetaRecord[_]
  override def getDBCollection[M <: TCM](query: Query[M, _, _]): MongoCollection[BsonDocument] = {
    query.meta.dba().getCollection(query.collectionName, bsonDocClass).withCodecRegistry(codecRegistry)
  }

  override def getPrimaryDBCollection[M <: TCM](query: Query[M, _, _]): MongoCollection[BsonDocument] = {
    getDBCollection(query)
  }

  protected def getPrimaryDBCollection(meta: CcMeta[_], collectionName: String): MongoCollection[BsonDocument] = {
    meta.dba().getCollection(collectionName, bsonDocClass).withCodecRegistry(codecRegistry)
  }

  /*override def getPrimaryDBCollection(record: MongoRecord[_]): MongoCollection[BsonDocument] = {
    getPrimaryDBCollection(record.meta, record.meta.collectionName)
  }*/

  override def getInstanceName[M <: TCM](query: Query[M, _, _]): String = {
    query.meta.dba().getName
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


class CcAsyncAdapter(dbCollectionFactory: AsyncBsonDBCollectionFactory[CcMeta[_]]) extends MongoAsyncBsonJavaDriverAdapter(dbCollectionFactory)



object CcAsyncAdapter extends CcAsyncAdapter(CcAsyncDBCollectionFactory)


class CcAsyncQueryExecutor(override val adapter: MongoAsyncBsonJavaDriverAdapter[CcMeta[_]]) extends AsyncBsonQueryExecutor[CcMeta[_]] {
  override def defaultWriteConcern = QueryHelpers.config.defaultWriteConcern
  override lazy val optimizer = new QueryOptimizer
  //M <: MongoRecord[_] with MongoMetaRecord[_], R]
  override protected def readSerializer[M <: CcMeta[_], R](meta: M, select: Option[MongoSelect[M, R]]): RogueBsonRead[R] = {
    new RogueBsonRead[R] {
      override def fromDocument(dbo: BsonDocument): R = select match {
        case Some(MongoSelect(fields, transformer)) if fields.isEmpty=>
          // A MongoSelect clause exists, but has empty fields. Return null.
          // This is used for .exists(), where we just want to check the number
          // of returned results is > 0.
          transformer(null)
        case Some(MongoSelect(fields, transformer)) =>
          //LiftQueryExecutorHelpers.setInstanceFieldFromDoc(inst, dbo, "_id")
          val values =
            fields.map(fld => {
              meta.reader(fld.field.name).read(dbo.get(fld.field.name))
              //val valueOpt = meta.fields
              //LiftQueryExecutorHelpers.setInstanceFieldFromDoc(inst, dbo, fld.field.name)
              //fld.valueOrDefault(valueOpt)
            })
          transformer(values)
        case None =>
          //TODO - better types !!!!
          meta.read(dbo).asInstanceOf[R]
      }
    }
  }

  override protected def writeSerializer[M <: CcMeta[_], R](meta: M): RogueBsonWrite[R] = {
    new RogueBsonWrite[R] {
      override def toDocument(record: R): BsonDocument = {
        meta.writeAnyRef(record.asInstanceOf[AnyRef]).asDocument()
      }
    }
  }
}

object CcAsyncQueryExecutor extends CcAsyncQueryExecutor(CcAsyncAdapter)
