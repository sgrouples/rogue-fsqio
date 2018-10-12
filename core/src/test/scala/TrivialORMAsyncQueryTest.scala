package io.fsq.rogue.test

import com.mongodb._
import com.mongodb.async.client
import com.mongodb.async.client.{ MongoClients, MongoCollection, MongoDatabase }
import io.fsq.rogue.MongoHelpers.{ AndCondition, MongoSelect }
import io.fsq.rogue.index.UntypedMongoIndex
import io.fsq.rogue.test.TrivialORM.{ Meta, Record }
import io.fsq.rogue.{ QueryOptimizer, RogueReadSerializer, RogueWriteSerializer, _ }
import org.bson.Document
import org.junit.{ Before, Test }
import org.specs2.matcher.JUnitMustMatchers

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object TrivialAsyncORMTests {

  def connectToMongo: client.MongoClient = {
    val (host, port) = Option(System.getProperty("default.mongodb.server")).map({ str =>
      val arr = str.split(':')
      (arr(0), arr(1).toInt)
    }).getOrElse(("localhost", 51101))
    //val opts = MongoClientOptions.builder().codecRegistry(TrivialSyncORM.codecRegistry).build()
    MongoClients.create(s"mongodb://${host}:${port}")

  }

  lazy val mongoAsync: async.client.MongoClient = {
    connectToMongo
  }

  def disconnectFromMongo = {
    mongoAsync.close
  }

  type MB = Meta[_]

  class MyDBCollectionFactory(dba: MongoDatabase) extends AsyncDBCollectionFactory[MB, Record] {
    val db = dba.withCodecRegistry(com.mongodb.MongoClient.getDefaultCodecRegistry)

    override def getDBCollection[M <: MB](query: Query[M, _, _]): MongoCollection[Document] = {
      db.getCollection(query.meta.collectionName)
    }

    override def getPrimaryDBCollection[M <: MB](query: Query[M, _, _]): MongoCollection[Document] = {
      db.getCollection(query.meta.collectionName)
    }

    override def getInstanceName[M <: MB](query: Query[M, _, _]): String = {
      db.getName
    }

    override def getIndexes[M <: MB](query: Query[M, _, _]): Option[List[UntypedMongoIndex]] = {
      None
    }

    override def getPrimaryDBCollection(record: Record): MongoCollection[Document] = ???

    override def getInstanceName(record: Record): String = ???
  }

  class MyQueryExecutor extends AsyncQueryExecutor[Meta[_], Record] {
    override val adapter = new MongoAsyncJavaDriverAdapter[Meta[_], Record](new MyDBCollectionFactory(mongoAsync.getDatabase("testAsync")))
    override val optimizer = new QueryOptimizer
    override val defaultWriteConcern: WriteConcern = WriteConcern.ACKNOWLEDGED

    /*
     protected def readSerializer[M <: MB, R](
                                            meta: M,
                                            select: Option[MongoSelect[M, R]]
                                          ): RogueReadSerializer[R]

  protected def writeSerializer(record: RB): RogueWriteSerializer[RB]

     */
    override protected def readSerializer[M <: Meta[_], R](
      meta: M,
      select: Option[MongoSelect[M, R]]): RogueReadSerializer[R] = new RogueReadSerializer[R] {
      override def fromDBObject(dbo: DBObject): R = select match {
        case Some(MongoSelect(fields, transformer, true, _)) if fields.isEmpty =>
          // A MongoSelect clause exists, but has empty fields. Return null.
          // This is used for .exists(), where we just want to check the number
          // of returned results is > 0.
          transformer(null)

        case Some(MongoSelect(fields, transformer, _, _)) =>
          transformer(fields.map(f => f.valueOrDefault(Option(dbo.get(f.field.name)))))

        case None =>
          meta.fromDBObject(dbo).asInstanceOf[R]
      }

      override def fromDocument(doc: Document): R = select match {
        case Some(MongoSelect(fields, transformer, true, _)) if fields.isEmpty =>
          // A MongoSelect clause exists, but has empty fields. Return null.
          // This is used for .exists(), where we just want to check the number
          // of returned results is > 0.
          transformer(null)

        case Some(MongoSelect(fields, transformer, _, _)) =>
          transformer(fields.map(f => f.valueOrDefault(Option(doc.get(f.field.name)))))

        case None =>
          meta.fromDocument(doc).asInstanceOf[R]
      }
    }

    override protected def writeSerializer(record: Record): RogueWriteSerializer[Record] = new RogueWriteSerializer[Record] {
      override def toDBObject(record: Record): DBObject = {
        ???
        ///record.meta.toDBObject(record)
      }

      override def toDocument(r: Record): Document = {
        ???
        //record.meta.toDocument(r)
      }
    }

  }

  object Implicits extends Rogue {
    implicit def meta2Query[M <: Meta[R], R](meta: M with Meta[R]): Query[M, R, InitialState] = {
      Query[M, R, InitialState](
        meta, meta.collectionName, None, None, None, None, None, AndCondition(Nil, None, None), None, None, None)
    }
  }

}

// TODO(nsanch): Everything in the rogue-lift tests should move here, except for the lift-specific extensions.
class TrivialAsyncORMQueryTest extends JUnitMustMatchers {
  val executor = new TrivialAsyncORMTests.MyQueryExecutor

  val oneS = 1 second

  import TrivialAsyncORMTests.Implicits._

  @Before
  def cleanUpMongo = {
    Await.ready(executor.bulkDelete_!!(SimpleRecord), oneS)
    ()
  }

  @Test
  def canBuildQuery: Unit = {
    (SimpleRecord: Query[SimpleRecord.type, SimpleRecord, InitialState]).toString() must_== """db.simple_records.find({ })"""
    SimpleRecord.where(_.a eqs 1).toString() must_== """db.simple_records.find({ "a" : 1 })"""
  }

  @Test
  def canExecuteQuery: Unit = {
    Await.result(executor.fetch(SimpleRecord.where(_.a eqs 1)), oneS) must_== Nil
    Await.result(executor.count(SimpleRecord), oneS) must_== 0
    Await.result(executor.exists(SimpleRecord), oneS) must_== false
  }

  @Test
  def canUpsertAndGetResults: Unit = {
    Await.result(executor.count(SimpleRecord), oneS) must_== 0

    val x = for {
      _ <- executor.upsertOne(SimpleRecord.modify(_.a setTo 1).and(_.b setTo "foo"))
      cnt <- executor.count(SimpleRecord)
      results <- executor.fetch(SimpleRecord.where(_.a eqs 1))
      e1 <- executor.exists(SimpleRecord.where(_.a eqs 1).select(_.a))
      r1 <- executor.fetch(SimpleRecord.where(_.a eqs 1).select(_.a))
      r2 <- executor.fetch(SimpleRecord.where(_.a eqs 1).select(_.b))
      r3 <- executor.fetch(SimpleRecord.where(_.a eqs 1).select(_.a, _.b))
    } yield {
      e1 must_== true
      cnt must_== 1
      results.size must_== 1
      results(0).a must_== 1
      results(0).b must_== "foo"
      r1 must_== List(Some(1))
      r2 must_== List(Some("foo"))
      r3 must_== List((Some(1), Some("foo")))
    }
    Await.ready(x, 10 seconds)

  }
}
