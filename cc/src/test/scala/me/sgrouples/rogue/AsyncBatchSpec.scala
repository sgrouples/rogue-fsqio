package me.sgrouples.rogue

import me.sgrouples.rogue.cc.{ MongoTestConn, UnitCallback }
import org.scalatest.{ AsyncFlatSpec, FlatSpec, Matchers }
import me.sgrouples.rogue.cc.macros._
import me.sgrouples.rogue.cc.CcRogue._

import scala.concurrent.Future

case class NumModel(_id: Int, bla: String)

class AsyncBatchSpec extends AsyncFlatSpec with Matchers {
  class NumModelMeta extends MCcMeta[NumModel, NumModelMeta]("nummodel") {
    @f val id = IntField("_id")
  }

  "batch async" should "batch with slice size and function" in {
    implicit val mongo = MongoTestConn.connectToMongo.getDatabase("nummodel")

    def reader(s: Iterable[Int]): Future[Seq[Seq[Int]]] = {
      Future.successful(Seq(s.toSeq))
    }
    val callback = new UnitCallback[Void]
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
}
