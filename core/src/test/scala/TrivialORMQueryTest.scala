// Copyright 2015 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue.test

import com.mongodb._
import io.fsq.rogue.MongoHelpers.{AndCondition, MongoSelect}
import io.fsq.rogue._
import io.fsq.rogue.index.UntypedMongoIndex
import io.fsq.rogue.test.TrivialORM.{Meta, Record}
import org.bson.Document
import org.junit.{Before, Test}
import org.specs2.matcher.JUnitMustMatchers


object TrivialSyncORM extends {

  lazy val mongo = {
    val (host, port) = Option(System.getProperty("default.mongodb.server")).map({ str =>
      val arr = str.split(':')
      (arr(0), arr(1).toInt)
    }).getOrElse(("localhost", 27017))
    new MongoClient(new ServerAddress(host, port))
  }

  type MB = Meta[_]
  class MyDBCollectionFactory(db: DB) extends DBCollectionFactory[MB, Record] {
    override def getDBCollection[M <: MB](query: Query[M, _, _]): DBCollection = {
      db.getCollection(query.meta.collectionName)
    }

    override def getPrimaryDBCollection[M <: MB](query: Query[M, _, _]): DBCollection = {
      db.getCollection(query.meta.collectionName)
    }

    override def getPrimaryDBCollection(record: Record): DBCollection = {
      db.getCollection(record.meta.collectionName)
    }

    override def getInstanceName[M <: MB](query: Query[M, _, _]): String = {
      db.getName
    }

    override def getInstanceName(record: Record): String = {
      db.getName
    }

    override def getIndexes[M <: MB](query: Query[M, _, _]): Option[Seq[UntypedMongoIndex]] = {
      None
    }
  }



  class MyQueryExecutor extends QueryExecutor[Meta[_], Record] {
    override val adapter = new MongoJavaDriverAdapter[Meta[_], Record](new MyDBCollectionFactory(mongo.getDB("test")))
    override val optimizer = new QueryOptimizer
    override val defaultWriteConcern: WriteConcern = WriteConcern.SAFE

    protected def readSerializer[M <: Meta[_], R](
                                                   meta: M,
                                                   select: Option[MongoSelect[M, R]]
                                                 ): RogueReadSerializer[R] = new RogueReadSerializer[R] {
      override def fromDBObject(dbo: DBObject): R = select match {
        case Some(MongoSelect(Nil, transformer)) =>
          // A MongoSelect clause exists, but has empty fields. Return null.
          // This is used for .exists(), where we just want to check the number
          // of returned results is > 0.
          transformer(null)

        case Some(MongoSelect(fields, transformer)) =>
          transformer(fields.map(f => f.valueOrDefault(Option(dbo.get(f.field.name)))))

        case None =>
          meta.fromDBObject(dbo).asInstanceOf[R]
      }

      override def fromDocument(doc: Document): R = select match {
        case Some(MongoSelect(Nil, transformer)) =>
          // A MongoSelect clause exists, but has empty fields. Return null.
          // This is used for .exists(), where we just want to check the number
          // of returned results is > 0.
          transformer(null)

        case Some(MongoSelect(fields, transformer)) =>
          transformer(fields.map(f => f.valueOrDefault(Option(doc.get(f.field.name)))))

        case None =>
          meta.fromDocument(doc).asInstanceOf[R]
      }
    }
    override protected def writeSerializer(record: Record): RogueWriteSerializer[Record] = new RogueWriteSerializer[Record] {
      override def toDBObject(record: Record): DBObject = { ???
        ///record.meta.toDBObject(record)
      }
      override def toDocument(r:Record): Document = { ???
        //record.meta.toDocument(r)
      }
    }
  }

  object Implicits extends Rogue {
    implicit def meta2Query[M <: Meta[R], R](meta: M with Meta[R]): Query[M, R, InitialState] = {
      Query[M, R, InitialState](
        meta, meta.collectionName, None, None, None, None, None, AndCondition(Nil, None), None, None, None)
    }
  }

}


// TODO: Everything in the rogue-lift tests should move here, except for the lift-specific extensions.
class TrivialORMQueryTest extends JUnitMustMatchers {
  val executor = new TrivialSyncORM.MyQueryExecutor

  import TrivialSyncORM.Implicits._

  @Before
  def cleanUpMongo = {
    executor.bulkDelete_!!(SimpleRecord)
  }

  @Test
  def canBuildQuery: Unit = {
    (SimpleRecord: Query[SimpleRecord.type, SimpleRecord, InitialState]).toString() must_== """db.simple_records.find({ })"""
    SimpleRecord.where(_.a eqs 1).toString() must_== """db.simple_records.find({ "a" : 1})"""
  }

  @Test
  def canExecuteQuery: Unit = {
    executor.fetch(SimpleRecord.where(_.a eqs 1)) must_== Nil
    executor.count(SimpleRecord) must_== 0
  }

  @Test
  def canUpsertAndGetResults: Unit = {
    executor.count(SimpleRecord) must_== 0

    executor.upsertOne(SimpleRecord.modify(_.a setTo 1).and(_.b setTo "foo"))

    executor.count(SimpleRecord) must_== 1

    val results = executor.fetch(SimpleRecord.where(_.a eqs 1))
    results.size must_== 1
    results(0).a must_== 1
    results(0).b must_== "foo"

    executor.fetch(SimpleRecord.where(_.a eqs 1).select(_.a)) must_== List(Some(1))
    executor.fetch(SimpleRecord.where(_.a eqs 1).select(_.b)) must_== List(Some("foo"))
    executor.fetch(SimpleRecord.where(_.a eqs 1).select(_.a, _.b)) must_== List((Some(1), Some("foo")))
  }
}
