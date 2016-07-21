package me.sgrouples.rogue.cc

import io.fsq.rogue.index.{IndexedRecord, UntypedMongoIndex}
import io.fsq.rogue.MongoHelpers.MongoSelect
import com.mongodb.DBObject
import com.mongodb.async.client.MongoCollection
import org.bson.{BsonArray, BsonDocument, BsonNull, BsonValue}
import io.fsq.rogue._
import org.bson.codecs.configuration.CodecRegistries

import scala.util.Try



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
          //TODO - optimze - extract readers for fields up, before using read serializer. this is super -ineffective
          //LiftQueryExecutorHelpers.setInstanceFieldFromDoc(inst, dbo, "_id")
          println(s"Whole DBO ${dbo} ${dbo.getClass}")
          val values =
            fields.map(fld => {
              val (bsonV, readArray) = readBsonVal(dbo, fld.field.name)
                //.get(fld.field.name)
              println(s"BsonV is ${bsonV}")
              println(s"field name is ${fld.field.name}")
              println(s"Field  is ${fld}")
              println(s"Field.field is ${fld.field}")
              val reader = meta.reader(fld.field)
              //TODO - does not in case reader is for non-array, and subselect returns array
              //if fld is optional, we might read null, that's why we need try-catch .. or readOption?
              fld.valueOrDefault(
                if(readArray) {
                  readOptArr(reader.readArray, bsonV.asArray())
                } else {
                  readOpt(reader.read, bsonV)
                }
              )
            })
          transformer(values)
        case None =>
          //TODO - better types !!!!
          meta.read(dbo).asInstanceOf[R]
      }
    }
  }


  /*
  Lift record version:
  fieldName.contains(".") match {
      case true =>
        val names = fieldName.split("\\.").toList.filter(_ != "$")
        setInstanceFieldFromDocList(instance, dbo, names)
      case false =>
        val fld: Box[LField[_, _]] = instance.fieldByName(fieldName)
        fld.flatMap (setLastFieldFromDoc(_, dbo, fieldName))
    }
   */


  private[this] def readOpt[T](reader: BsonValue => T, v: BsonValue): Option[T] = {
    if(v.isNull) None
    else Option(reader(v))
  }

  private[this] def readOptArr[T](reader: BsonArray => Seq[T], v: BsonArray): Option[Seq[T]] = {
    if(v.isNull) None
    else Option(reader(v))
  }

  private[this] def readBsonVal(dbo: BsonDocument, fldName: String):(BsonValue, Boolean) = {
    val parts = fldName.split('.')
    println(s"parts fldName ${fldName} len ${parts.length} ${parts}")
    var i = 0
    var needArray = false
    var d:BsonValue = dbo
    while(i < parts.length) {
      if(d==null) {
        i = parts.length
        d = BsonNull.VALUE
      } else {
        val key = parts(i)
        if (key == "$") {
          //TODO - probably it means that array is needed
          i = i+1
        } else {
          if (d.isArray) {
            val r = new BsonArray()
            val it = d.asArray().iterator()
            while (it.hasNext) {
              r.add(it.next().asDocument().get(key))
            }
            d = r
            needArray = true
          } else {
            d = d.asDocument().get(key)
          }
          println(s"Getting ${parts(i)} from ${d}")
          println(s"Got ${d}")
          i = i + 1
        }
      }
    }
    println(s"Returning ${d} ${d.getBsonType}")
    (d, needArray)
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
