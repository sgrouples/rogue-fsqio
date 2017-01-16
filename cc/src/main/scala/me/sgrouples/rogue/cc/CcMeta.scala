package me.sgrouples.rogue.cc

import com.mongodb.client.MongoDatabase
import com.mongodb.client.model.IndexOptions
import io.fsq.field.Field
import me.sgrouples.rogue._
import me.sgrouples.rogue.naming.{ LowerCase, NamingStrategy }
import org.bson.types.ObjectId
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
  type OwnerType = this.type
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

  def intField(name: String) = new IntField(name, this)
  def optIntField(name: String) = new OptIntField(name, this)
  def longField(name: String) = new LongField(name, this)
  def optLongField(name: String) = new OptLongField(name, this)
  def doubleField(name: String) = new DoubleField(name, this)
  def optDoubleField(name: String) = new OptDoubleField(name, this)
  def stringField(name: String) = new StringField(name, this)
  def optStringField(name: String) = new OptStringField(name, this)
  def objectIdField(name: String) = new ObjectIdField(name, this)
  def optObjectIdField(name: String) = new OptObjectIdField(name, this)
  def objectSubtypeIdField[KeyType <: ObjectId](name: String) = new ObjectIdSubtypeField[KeyType, this.type](name, this)
  def optObjectSubtypeIdField[KeyType <: ObjectId](name: String) = new OptObjectIdSubtypeField[KeyType, this.type](name, this)
  def uuidIdField(name: String) = new UUIDIdField(name, this)
  def optUuidIdField(name: String) = new OptUUIDIdField(name, this)
  def instantField(name: String) = new InstantField(name, this)
  def optInstantField(name: String) = new OptInstantField(name, this)
  def localDateTimeField(name: String) = new LocalDateTimeField(name, this)
  def optLocalDateTimeField(name: String) = new OptLocalDateTimeField(name, this)
  def booleanField(name: String) = new BooleanField(name, this)
  def optBooleanField(name: String) = new OptBooleanField(name, this)
  def enumField[T <: Enumeration](name: String, e: T) = new EnumField[T, this.type](name, this)(e)
  def optEnumField[T <: Enumeration](name: String, e: T) = new OptEnumField[T, this.type](name, this)(e)
  def enumIdField[T <: Enumeration](name: String, e: T) = new EnumIdField[T, this.type](name, this)(e)
  def optEnumIdField[T <: Enumeration](name: String, e: T) = new OptEnumIdField[T, this.type](name, this)(e)
  def listField[V](name: String) = new ListField[List[V], this.type](name, this)
  def optListField[V](name: String) = new OptListField[List[V], this.type](name, this)
  def arrayField[V: ClassTag](name: String) = new ArrayField[Array[V], this.type](name, this)
  def optArrayField[V: ClassTag](name: String) = new OptArrayField[Array[V], this.type](name, this)
  def cClassField[C, MC <: CcMeta[C]](name: String, childMeta: MC) = new CClassField[C, MC, this.type](name, childMeta, this)
  def optCclassField[C, MC <: CcMeta[C]](name: String, childMeta: MC) = {
    type X = childMeta.R
    new OptCClassField[X, MC, this.type](name, childMeta, this)
  }
  def cClassRequiredField[C, MC <: CcMeta[C]](name: String, childMeta: MC, default: C) = new CClassRequiredField[C, MC, this.type](name, childMeta, default, this)
  /*def cClassListField[MC <: CcMeta[_]](name: String, childMeta: MC) = {
    new CClassListField[childMeta.R, MC, this.type](name, childMeta, this)
  }*/
  def optCclassListField[C, MC <: CcMeta[C]](name: String, childMeta: MC) = new OptCClassListField[C, MC, this.type](name, childMeta, this)
  def cClassArrayField[C: ClassTag, MC <: CcMeta[C]](name: String, childMeta: MC) = new CClassArrayField[C, MC, this.type](name, childMeta, this)
  def mapField[V](name: String) = new MapField(name, this)
  def optMapField[V](name: String) = new OptMapField(name, this)

}
