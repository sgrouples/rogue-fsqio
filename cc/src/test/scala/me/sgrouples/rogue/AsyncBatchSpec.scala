package me.sgrouples.rogue

import java.util.concurrent.atomic.AtomicInteger
import me.sgrouples.rogue.cc.MongoTestConn
import me.sgrouples.rogue.cc.macros._
import me.sgrouples.rogue.cc.CcRogue._
import munit.FunSuite

import scala.concurrent.Future

case class NumModel(_id: Int, bla: String)

class AsyncBatchSpec extends FunSuite {
  class NumModelMeta extends MCcMeta[NumModel, NumModelMeta]("nummodel") {
    @f val id = IntField("_id")
  }

 /* "batch async" should "batch with slice size and function" in {
    implicit val mongo = MongoTestConn.connectToMongo.getDatabase("nummodel")

    def reader(s: Iterable[Int]): Future[Seq[Seq[Int]]] = {
      Future.successful(Seq(s.toSeq))
    }
    val NumModels = new NumModelMeta
    val nums = for (i <- 1 to 90) yield NumModel(i, "bla")
    mongo.getCollection("nummodel").drop(callback)
    for {
      _ <- callback.future
      _ <- NumModels.insertManyAsync(nums)
      processed <- NumModels.select(_.id).orderDesc(_.id).batchAsync(reader, 20)
    } yield {
      MongoTestConn.disconnectFromMongo
      //90 / 5 - 5 groups
      processed.length should ===(5)
      val topPiece = for { i <- 90 to 71 by -1 } yield i
      val bottomPiece = for { i <- 10 to 1 by -1 } yield i
      processed.head should ===(topPiece)
      processed.last should ===(bottomPiece)
      processed.flatten.length should ===(90)
    }
  }

  "batch async" should "fail if given processing function fails" in {
    implicit val mongo = MongoTestConn.connectToMongo.getDatabase("nummodel")
    val counter = new AtomicInteger(0)

    def reader(s: Iterable[Int]): Future[Seq[Seq[Int]]] = {
      //I want to first batch to succeed and the second one to fail
      if (counter.incrementAndGet() % 2 == 1)
        Future.successful(Seq(s.toSeq))
      else
        Future.failed(new UnsupportedOperationException("Some funky exception to test failures"))
    }

    val callback = new UnitCallback[Void]
    val NumModels = new NumModelMeta
    val nums = for (i <- 1 to 90) yield NumModel(i, "bla")
    mongo.getCollection("nummodel").drop(callback)
    for {
      _ <- callback.future
      _ <- NumModels.insertManyAsync(nums)
      _ <- recoverToSucceededIf[UnsupportedOperationException](NumModels.select(_.id).orderDesc(_.id).batchAsync(reader, 20))
    } yield {
      MongoTestConn.disconnectFromMongo
      succeed
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    MongoTestConn.disconnectFromMongo
  }
  */
}
