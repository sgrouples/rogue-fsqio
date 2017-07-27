// Copyright 2012 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue.lift

import com.mongodb.{ DBCollection, DBObject }
import io.fsq.rogue.{ DBCollectionFactory, MongoJavaDriverAdapter, Query, QueryExecutor, QueryHelpers, QueryOptimizer, RogueReadSerializer, RogueWriteSerializer }
import io.fsq.rogue.MongoHelpers.MongoSelect
import io.fsq.rogue.index.{ IndexedRecord, UntypedMongoIndex }
import net.liftweb.common.Box
import net.liftweb.mongodb.MongoDB
import net.liftweb.mongodb.record.{ BsonRecord, MongoMetaRecord, MongoRecord }
import net.liftweb.mongodb.record.field.BsonRecordField
import org.bson.Document
import org.bson.types.BasicBSONList
import java.util

object LiftDBCollectionFactory extends DBCollectionFactory[MongoRecord[_] with MongoMetaRecord[_], MongoRecord[_]] {
  override def getDBCollection[M <: MongoRecord[_] with MongoMetaRecord[_]](query: Query[M, _, _]): DBCollection = {
    MongoDB.use(query.meta.connectionIdentifier) { db =>
      db.getCollection(query.collectionName)
    }
  }
  protected def getPrimaryDBCollection(meta: MongoMetaRecord[_], collectionName: String): DBCollection = {
    MongoDB.use(meta /* TODO: .master*/ .connectionIdentifier) { db =>
      db.getCollection(collectionName)
    }
  }
  override def getPrimaryDBCollection[M <: MongoRecord[_] with MongoMetaRecord[_]](query: Query[M, _, _]): DBCollection = {
    getPrimaryDBCollection(query.meta, query.collectionName)
  }
  override def getPrimaryDBCollection(record: MongoRecord[_]): DBCollection = {
    getPrimaryDBCollection(record.meta, record.meta.collectionName)
  }
  override def getInstanceName[M <: MongoRecord[_] with MongoMetaRecord[_]](query: Query[M, _, _]): String = {
    query.meta.connectionIdentifier.toString
  }
  override def getInstanceName(record: MongoRecord[_]): String = {
    record.meta.connectionIdentifier.toString
  }

  /**
   * Retrieves the list of indexes declared for the record type associated with a
   * query. If the record type doesn't declare any indexes, then returns None.
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

class LiftAdapter(dbCollectionFactory: DBCollectionFactory[MongoRecord[_] with MongoMetaRecord[_], MongoRecord[_]])
  extends MongoJavaDriverAdapter(dbCollectionFactory)

object LiftAdapter extends LiftAdapter(LiftDBCollectionFactory)

class LiftQueryExecutor(
    override val adapter: MongoJavaDriverAdapter[MongoRecord[_] with MongoMetaRecord[_], MongoRecord[_]]
) extends QueryExecutor[MongoRecord[_] with MongoMetaRecord[_], MongoRecord[_]] {
  override def defaultWriteConcern = QueryHelpers.config.defaultWriteConcern
  override lazy val optimizer = new QueryOptimizer

  override protected def readSerializer[M <: MongoRecord[_] with MongoMetaRecord[_], R](
    meta: M,
    select: Option[MongoSelect[M, R]]
  ): RogueReadSerializer[R] = {
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

object LiftQueryExecutor extends LiftQueryExecutor(LiftAdapter)

object LiftQueryExecutorHelpers {
  import net.liftweb.record.{ Field => LField }

  def setInstanceFieldFromDboList(instance: BsonRecord[_], dbo: DBObject, fieldNames: List[String]): Option[_] = {
    fieldNames match {
      case last :: Nil =>
        val fld: Box[LField[_, _]] = instance.fieldByName(last)
        fld.flatMap(setLastFieldFromDbo(_, dbo, last))
      case name :: rest =>
        val fld: Box[LField[_, _]] = instance.fieldByName(name)
        dbo.get(name) match {
          case obj: DBObject => fld.flatMap(setFieldFromDbo(_, obj, rest))
          case list: BasicBSONList => fallbackValueFromDbObject(dbo, fieldNames)
          case null => None
        }
      case Nil => throw new UnsupportedOperationException("was called with empty list, shouldn't possibly happen")
    }
  }

  def setInstanceFieldFromDocList(instance: BsonRecord[_], dbo: Document, fieldNames: List[String]): Option[_] = {
    fieldNames match {
      case last :: Nil =>
        val fld: Box[LField[_, _]] = instance.fieldByName(last)
        fld.flatMap(setLastFieldFromDoc(_, dbo, last))
      case name :: rest =>
        val fld: Box[LField[_, _]] = instance.fieldByName(name)
        dbo.get(name) match {
          case obj: Document => fld.flatMap(setFieldFromDoc(_, obj, rest))
          //ArrayList
          case list: util.ArrayList[_] => fallbackValueFromDoc(dbo, fieldNames)
          case null => None
        }
      case Nil => throw new UnsupportedOperationException("was called with empty list, shouldn't possibly happen")
    }
  }

  def setFieldFromDbo(field: LField[_, _], dbo: DBObject, fieldNames: List[String]): Option[_] = {
    if (field.isInstanceOf[BsonRecordField[_, _]]) {
      val brf = field.asInstanceOf[BsonRecordField[_, _]]
      val inner = brf.value.asInstanceOf[BsonRecord[_]]
      setInstanceFieldFromDboList(inner, dbo, fieldNames)
    } else {
      fallbackValueFromDbObject(dbo, fieldNames)
    }
  }

  def setFieldFromDoc(field: LField[_, _], dbo: Document, fieldNames: List[String]): Option[_] = {
    if (field.isInstanceOf[BsonRecordField[_, _]]) {
      val brf = field.asInstanceOf[BsonRecordField[_, _]]
      val inner = brf.value.asInstanceOf[BsonRecord[_]]
      setInstanceFieldFromDocList(inner, dbo, fieldNames)
    } else {
      fallbackValueFromDoc(dbo, fieldNames)
    }
  }

  def setLastFieldFromDoc(field: LField[_, _], dbo: Document, fieldName: String): Option[_] = {
    field.setFromAny(dbo.get(fieldName)).toOption
  }

  def setLastFieldFromDbo(field: LField[_, _], dbo: DBObject, fieldName: String): Option[_] = {
    field.setFromAny(dbo.get(fieldName)).toOption
  }

  def setInstanceFieldFromDbo(instance: MongoRecord[_], dbo: DBObject, fieldName: String): Option[_] = {
    fieldName.contains(".") match {
      case true =>
        val names = fieldName.split("\\.").toList.filter(_ != "$")
        setInstanceFieldFromDboList(instance, dbo, names)
      case false =>
        val fld: Box[LField[_, _]] = instance.fieldByName(fieldName)
        fld.flatMap(setLastFieldFromDbo(_, dbo, fieldName))
    }
  }

  def setInstanceFieldFromDoc(instance: MongoRecord[_], dbo: Document, fieldName: String): Option[_] = {
    fieldName.contains(".") match {
      case true =>
        val names = fieldName.split("\\.").toList.filter(_ != "$")
        setInstanceFieldFromDocList(instance, dbo, names)
      case false =>
        val fld: Box[LField[_, _]] = instance.fieldByName(fieldName)
        fld.flatMap(setLastFieldFromDoc(_, dbo, fieldName))
    }
  }

  def fallbackValueFromDbObject(dbo: DBObject, fieldNames: List[String]): Option[_] = {
    import scala.collection.JavaConverters._
    Box.!!(fieldNames.foldLeft(dbo: Object)((obj: Object, fieldName: String) => {
      obj match {
        case dbl: BasicBSONList =>
          dbl.asScala.map(_.asInstanceOf[DBObject]).map(_.get(fieldName)).toList
        case dbo: DBObject =>
          dbo.get(fieldName)
        case null => null
      }
    })).toOption
  }

  def fallbackValueFromDoc(dbo: Document, fieldNames: List[String]): Option[_] = {
    import scala.collection.JavaConverters._
    Box.!!(fieldNames.foldLeft(dbo: Object)((obj: Object, fieldName: String) => {
      obj match {
        case dbl: util.ArrayList[_] =>
          dbl.asScala.map(_.asInstanceOf[Document]).map(_.get(fieldName)).toList
        case dbo: Document =>
          dbo.get(fieldName)
        case null => null
      }
    })).toOption
  }
}
