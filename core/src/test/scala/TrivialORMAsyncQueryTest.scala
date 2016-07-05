package io.fsq.rogue.test

import com.mongodb._
import com.mongodb.async.client.{MongoClients, MongoCollection, MongoDatabase}
import io.fsq.field.OptionalField
import io.fsq.rogue.{MongoJavaDriverAdapter, QueryExecutor, QueryOptimizer, RogueReadSerializer, RogueWriteSerializer, _}
import io.fsq.rogue.MongoHelpers.{AndCondition, MongoSelect}
import io.fsq.rogue.index.UntypedMongoIndex
import org.bson.Document
import org.junit.{Before, Test}
import org.specs2.matcher.JUnitMustMatchers

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object TrivialAsyncORM {
  trait Record {
    type Self >: this.type <: Record
    def meta: Meta[Self]
  }

  trait Meta[R] {
    def collectionName: String
    def fromDBObject(dbo: DBObject): R
    def fromDocument(doc: Document): R
  }

  val mongo: async.client.MongoClient = {
    /*    val (host, port) = Option(System.getProperty("default.mongodb.server")).map({ str =>
    val arr = str.split(':')
    (arr(0), arr(1).toInt)
  }).getOrElse(("localhost", 27017))
  new MongoClient(new ServerAddress(host, port))
  */
  val MongoPort = Option(System.getenv("MONGO_PORT")).map(_.toInt).getOrElse(37648)
    MongoClients.create(s"mongodb://localhost:${MongoPort}")
  }

  def disconnectFromMongo = {
    mongo.close
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

    override def getPrimaryDBCollection(record: Record): MongoCollection[Record] = ???

    override def getInstanceName(record: Record): String = ???
  }

  class MyQueryExecutor extends AsyncQueryExecutor[Meta[_]] {
    override val adapter = new MongoAsyncJavaDriverAdapter[Meta[_]](new MyDBCollectionFactory(mongo.getDatabase("test")))
    override val optimizer = new QueryOptimizer
    override val defaultWriteConcern: WriteConcern = WriteConcern.ACKNOWLEDGED

    protected def serializer[M <: Meta[_], R](
                                               meta: M,
                                               select: Option[MongoSelect[M, R]]
                                             ): RogueSerializer[R] = new RogueSerializer[R] {
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
  }

  object Implicits extends Rogue {
    implicit def meta2Query[M <: Meta[R], R](meta: M with Meta[R]): Query[M, R, InitialState] = {
      Query[M, R, InitialState](
        meta, meta.collectionName, None, None, None, None, None, AndCondition(Nil, None), None, None, None)
    }
  }
}

case class SimpleARecord(a: Int, b: String)

object SimpleARecord extends TrivialAsyncORM.Meta[SimpleARecord] {
  val a = new OptionalField[Int, SimpleARecord.type] { override val owner = SimpleARecord; override val name = "a" }
  val b = new OptionalField[String, SimpleARecord.type] { override val owner = SimpleARecord; override val name = "b" }

  override val collectionName = "simplea_records"
  override def fromDBObject(dbo: DBObject): SimpleARecord = {
    new SimpleARecord(dbo.get(a.name).asInstanceOf[Int], dbo.get(b.name).asInstanceOf[String])
  }
  override def fromDocument(dbo: Document): SimpleARecord = {
    new SimpleARecord(dbo.get(a.name).asInstanceOf[Int], dbo.get(b.name).asInstanceOf[String])
  }

}


// TODO(nsanch): Everything in the rogue-lift tests should move here, except for the lift-specific extensions.
class TrivialAsyncORMQueryTest extends JUnitMustMatchers {
  val executor = new TrivialAsyncORM.MyQueryExecutor

  val oneS = 1 second

  import TrivialAsyncORM.Implicits._

  @Before
  def cleanUpMongo = {
    Await.ready(executor.bulkDelete_!!(SimpleARecord), oneS)
    ()
  }

  @Test
  def canBuildQuery: Unit = {
    (SimpleARecord: Query[SimpleARecord.type, SimpleARecord, InitialState]) .toString() must_== """db.simplea_records.find({ })"""
    SimpleARecord.where(_.a eqs 1)                                        .toString() must_== """db.simplea_records.find({ "a" : 1})"""
  }

  @Test
  def canExecuteQuery: Unit = {
    Await.result(executor.fetch(SimpleARecord.where(_.a eqs 1)), oneS) must_== Nil
    Await.result(executor.count(SimpleARecord), oneS) must_== 0
    Await.result(executor.exists(SimpleARecord), oneS) must_== false
  }

  @Test
  def canUpsertAndGetResults: Unit = {
    Await.result(executor.count(SimpleARecord), oneS) must_== 0

    val x = for {
      _ <- executor.upsertOne(SimpleARecord.modify(_.a setTo 1).and(_.b setTo "foo"))
      cnt <- executor.count(SimpleARecord)
      results <- executor.fetch(SimpleARecord.where(_.a eqs 1))
      e1 <- executor.exists(SimpleARecord.where(_.a eqs 1).select(_.a))
      r1 <- executor.fetch(SimpleARecord.where(_.a eqs 1).select(_.a))
      r2 <- executor.fetch(SimpleARecord.where(_.a eqs 1).select(_.b))
      r3 <- executor.fetch(SimpleARecord.where(_.a eqs 1).select(_.a, _.b))
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