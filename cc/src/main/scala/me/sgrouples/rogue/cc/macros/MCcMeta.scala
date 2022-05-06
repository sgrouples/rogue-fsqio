package me.sgrouples.rogue.cc.macros

import org.mongodb.scala.*
import io.fsq.field.Field
import io.fsq.rogue.index.MongoIndex
import me.sgrouples.rogue.BsonFormat
import me.sgrouples.rogue.cc.{CcMeta, QueryFieldHelpers}
import me.sgrouples.rogue.naming.{LowerCase, NamingStrategy}
import org.bson.{BsonDocument, BsonInt32, BsonValue}
import org.mongodb.scala.model.IndexOptions

import scala.concurrent.Future
import scala.reflect.ClassTag

//TODO - memoize field readers
class MCcMeta[RecordType, OwnerType <: CcMeta[RecordType]](collName: String)(
    implicit val macroGen: MacroBsonFormat[RecordType]
) extends QueryFieldHelpers[OwnerType]
    with CcMeta[RecordType]
    with MacroNamesResolver[RecordType] {
  requires: OwnerType =>
  def this(namingStrategy: NamingStrategy = LowerCase)(implicit
      macroGen: MacroBsonFormat[RecordType],
      classTag: ClassTag[RecordType]
  ) = {
    this(namingStrategy[RecordType])(macroGen)
  }
  import me.sgrouples.rogue.cc.Waiter._

  override def collectionName: String = collName

  override def reader(field: Field[_, _]): BsonFormat[_] = {
    val fieldName = field.name.replaceAll("\\.\\$", "")
    val r = macroGen.flds.get(fieldName)
    r.orElse(starReader(fieldName)).getOrElse {
      throw new RuntimeException(
        s"No reader for field ${fieldName}, available keys ${macroGen.flds.keys.mkString(",")}"
      )
    }
  }

  //special case for Map formats
  private[this] def starReader(fieldName: String): Option[BsonFormat[_]] = {
    val i = fieldName.lastIndexOf('.')
    if (i > 0) {
      val newName = fieldName.substring(0, i + 1) + "*"
      macroGen.flds.get(newName)
    } else None
  }

  override def read(b: BsonValue): RecordType = macroGen.read(b)

  override def write(t: RecordType): BsonValue = macroGen.write(t)

  override def writeAnyRef(t: AnyRef): BsonDocument =
    macroGen.write(t.asInstanceOf[RecordType]).asDocument()

  /** @param indexTuples
    *   sequence of (name, int)
    * @param opts
    *   IndexOptions- from mongo
    * @return
    *   - index name
    */
  @deprecated("Use `MongoIndex` version instead", "2019/02/15")
  def createIndex(indexTuples: Seq[(String, Int)], opts: IndexOptions)(implicit
      dbs: MongoDatabase
  ): String =
    waitForFuture(createIndexAsync(indexTuples, opts))

  /** ``
    * @param indexTuples
    *   sequence of (name, int)
    * @param opts
    *   IndexOptions- from mongo
    * @return
    *   - index name
    */
  @deprecated("Use `MongoIndex` version instead", "2019/02/15")
  def createIndexAsync(indexTuples: Seq[(String, Int)], opts: IndexOptions)(
      implicit dba: MongoDatabase
  ): Future[String] = {
    val keys = new BsonDocument()
    indexTuples.foreach { case (k, v) => keys.append(k, new BsonInt32(v)) }
    dba.getCollection(collectionName).createIndex(keys, opts).toFuture()
  }

  /** @param indexTuple
    *   - field, order tuple
    * @param opts
    *   - IndexOptions
    * @return
    *   index name (string)
    */
  @deprecated("Use `MongoIndex` version instead", "2019/02/15")
  def createIndex(
      indexTuple: (String, Int),
      opts: IndexOptions = new IndexOptions()
  )(implicit dbs: MongoDatabase): String =
    createIndex(Seq(indexTuple), opts)

  @deprecated("Use `MongoIndex` version instead", "2019/02/15")
  def createIndexAsync(
      indexTuple: (String, Int),
      opts: IndexOptions = new IndexOptions()
  )(implicit dba: MongoDatabase): Future[String] =
    createIndexAsync(Seq(indexTuple), opts)

  @deprecated("Use `MongoIndex` version instead", "2019/02/15")
  def createIndex(indexTuples: Seq[(String, Int)])(implicit
      dbs: MongoDatabase
  ): String = createIndex(indexTuples, new IndexOptions())

  @deprecated("Use `MongoIndex` version instead", "2019/02/15")
  def createIndexAsync(indexTuples: Seq[(String, Int)])(implicit
      dba: MongoDatabase
  ): Future[String] =
    createIndexAsync(indexTuples, new IndexOptions())

  /** Index creation without any checks or enforcements - might lock entire
    * collection if `background` flag is not specified
    * @param index
    *   MongoIndex to create
    * @param opts
    *   IndexOptions- from mongo
    * @return
    *   - index name
    */
  def createIndexUnsafe(
      index: MongoIndex[OwnerType],
      opts: IndexOptions = new IndexOptions()
  )(implicit dbs: MongoDatabase): String = {
    val indexBson = index.asBsonDocument
    waitForFuture(
      dbs.getCollection(collectionName).createIndex(indexBson, opts).toFuture()
    )
  }

  /** Index creation without any checks or enforcements - might lock entire
    * collection if `background` flag is not specified
    * @param index
    *   MongoIndex to create
    * @param opts
    *   IndexOptions- from mongo
    * @return
    *   - index name
    */
  def createIndexUnsafeAsync(
      index: MongoIndex[OwnerType],
      opts: IndexOptions = new IndexOptions()
  )(implicit dba: MongoDatabase): Future[String] = {
    val indexBson = index.asBsonDocument
    dba.getCollection(collectionName).createIndex(indexBson, opts).toFuture()
  }

  /** "Safe" version of index creation - it forces index to be created in the
    * background
    */
  def createIndex(index: MongoIndex[OwnerType], opts: IndexOptions)(implicit
      dbs: MongoDatabase
  ): String = {
    createIndexUnsafe(index, opts.background(true))
  }
  def createIndex(index: MongoIndex[OwnerType])(implicit
      dbs: MongoDatabase
  ): String = createIndex(index, new IndexOptions())

  /** "Safe" version of index creation - it forces index to be created in the
    * background
    */
  def createIndexAsync(index: MongoIndex[OwnerType], opts: IndexOptions)(
      implicit dba: MongoDatabase
  ): Future[String] = {
    createIndexUnsafeAsync(index, opts.background(true))
  }
  def createIndexAsync(
      index: MongoIndex[OwnerType]
  )(implicit dba: MongoDatabase): Future[String] = {
    createIndexAsync(index, new IndexOptions())
  }

}
