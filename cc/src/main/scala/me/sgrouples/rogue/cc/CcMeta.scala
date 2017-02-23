package me.sgrouples.rogue.cc

import com.mongodb.client.MongoDatabase
import com.mongodb.client.model.IndexOptions
import io.fsq.field.Field
import me.sgrouples.rogue.BsonFormat
import me.sgrouples.rogue.naming.{ LowerCase, NamingStrategy }
import org.bson.{ BsonDocument, BsonInt32, BsonValue }

import scala.reflect.ClassTag

trait CcMetaLike[-T] {
  type R
}

trait CcMeta[T] extends CcMetaLike[T] {
  type R = T
  //capture T to be able to cast to it
  //type R >: T
  def collectionName: String

  def read(b: BsonValue): T
  def write(t: T): BsonValue
  //TODO - how to make it play nice with types?
  def writeAnyRef(t: AnyRef): BsonDocument

  def reader(field: Field[_, _]): BsonFormat[_]
}

class RCcMeta[T](collName: String)(implicit f: BsonFormat[T]) extends CcMeta[T] {

  def this(namingStrategy: NamingStrategy = LowerCase)(implicit f: BsonFormat[T], classTag: ClassTag[T]) {
    this(namingStrategy[T])
  }

  override def collectionName: String = collName

  override def reader(field: Field[_, _]): BsonFormat[_] = {
    val fieldName = field.name.replaceAll("\\.\\$", "")
    // if field.isInstanceOf[]
    val r = f.flds.get(fieldName)
    r.orElse(starReader(fieldName)).getOrElse {
      throw new RuntimeException(s"No reader for field ${fieldName}, available keys ${f.flds.keys.mkString(",")}")
    }
  }

  //special case for Map formats
  private[this] def starReader(fieldName: String): Option[BsonFormat[_]] = {
    val i = fieldName.lastIndexOf('.')
    if (i > 0) {
      val newName = fieldName.substring(0, i + 1) + "*"
      f.flds.get(newName)
    } else None
  }

  override def read(b: BsonValue): T = f.read(b)

  override def write(t: T): BsonValue = f.write(t)

  override def writeAnyRef(t: AnyRef): BsonDocument = f.write(t.asInstanceOf[T]).asDocument()

  /**
   * @param indexTuples  sequence of (name, int)
   * @param opts IndexOptions- from mongo
   * @return  - index name
   */
  def createIndex(indexTuples: Seq[(String, Int)], opts: IndexOptions)(implicit dbs: MongoDatabase): String = {
    val keys = new BsonDocument()
    indexTuples.foreach { case (k, v) => keys.append(k, new BsonInt32(v)) }
    dbs.getCollection(collectionName).createIndex(keys, opts)
  }

  /**
   * @param indexTuple - field, order tuple
   * @param opts - IndexOptions
   * @return index name (string)
   */
  def createIndex(indexTuple: (String, Int), opts: IndexOptions = new IndexOptions())(implicit dbs: MongoDatabase): String =
    createIndex(Seq(indexTuple), opts)

  def createIndex(indexTuples: Seq[(String, Int)])(implicit dbs: MongoDatabase): String = createIndex(indexTuples, new IndexOptions())

}

/**
  * Rogue Case Class Meta Extended, awesome name!
  *
  * @param collName
  * @param formats
  * @tparam RecordType
  * @tparam OwnerType
  */

class RCcMetaExt[RecordType, OwnerType <: RCcMeta[RecordType]](collName: String)(implicit formats: BsonFormat[RecordType])
    extends RCcMeta[RecordType](collName)(formats)
    with QueryFieldHelpers[OwnerType] { requires: OwnerType =>

  def this(
    namingStrategy: NamingStrategy = LowerCase
  )(implicit formats: BsonFormat[RecordType], classTag: ClassTag[RecordType]) {
    this(namingStrategy[RecordType])(formats)
  }
}
