package me.sgrouples.rogue.cc

import org.bson.types.ObjectId
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll, Matchers }
import me.sgrouples.rogue.BsonFormats._
import me.sgrouples.rogue.cc.CcRogue._
import scala.collection.Seq

case class Cont(
  _id: ObjectId,
  lst: Seq[ObjectId])

class M extends RCcMetaExt[Cont, M]("cont") {
  val id = ObjectIdField("_id")
  val lst = ListField[ObjectId]
}

class ListFieldCaseSpec extends AsyncFlatSpec with Matchers with BeforeAndAfterAll {

  override def beforeAll() = {
    super.beforeAll()
    val m = MongoTestConn.connectToMongo
  }

  override def afterAll() = {
    super.afterAll()
    val m = MongoTestConn.disconnectFromMongo
  }

  "inner list in select" should "work" in {
    implicit val db = MongoTestConn.client.get.getDatabase("conttest")
    val me = new M
    for {
      _ <- me.insertManyAsync(Seq(Cont(new ObjectId, List(new ObjectId, new ObjectId)), Cont(new ObjectId(), List(new ObjectId(), new ObjectId()))))
      res <- me.select(_.id, _.lst).fetchAsync()
    } yield {
      noException should be thrownBy {
        res.map {
          case (id, lst) => lst
        }
      }
      assert(res.nonEmpty, "res ok")
    }
  }

}
