package me.sgrouples.rogue.cc

import me.sgrouples.rogue._
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, MustMatchers }
import org.bson.types.ObjectId
import me.sgrouples.rogue.cc.CcRogue._
import org.scalatest.concurrent.{ Futures, ScalaFutures }

import scala.concurrent.Future
import scala.util.{ Failure, Try }

/**
 * Created by mwielocha on 03/08/16.
 */

case class ACaseClass(
  anId: ObjectId,
  aString: String,
  anInt: Int,
  aLong: Long
)

class RCcAutoMetaSpec extends FlatSpec with MustMatchers with BeforeAndAfterEach with ScalaFutures {

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    val m = MongoTestConn.connectToMongo
    CcMongo.defineDb("default", m, "rogue-test-")
  }

  override protected def afterEach(): Unit = {
    super.afterEach()
    MongoTestConn.disconnectFromMongoSync
  }

  "CcMeta macro" should "create a proper cc meta from a cc" in {

    import scala.concurrent.ExecutionContext.Implicits.global

    import me.sgrouples.rogue.BsonFormats._

    val CaseClasses = RCcAutoMeta[ACaseClass]("case_classes")

    val id = new ObjectId

    val foo = ACaseClass(id, "test", 1, 1L)

    val insert = CaseClasses.insertOneAsync(foo)

    val get = CaseClasses
      .where(_.anId eqs id)
      .getAsync()

    val r = {
      for {
        _ <- insert
        f <- get
      } yield f
    }

    r.futureValue mustBe Some(foo)
  }
}
