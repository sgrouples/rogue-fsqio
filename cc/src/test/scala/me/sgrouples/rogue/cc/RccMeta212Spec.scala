package me.sgrouples.rogue.cc

import java.time.Instant

import me.sgrouples.rogue.BsonFormats._
import org.bson.types.ObjectId
import org.scalatest.{ FlatSpec, Matchers }

trait QueryById[M] {
  requires: QueryFieldHelpers[M] =>

  val thisIsWhatFails = ObjectIdField("_id")

}

case class UserBlock(
  _id: ObjectId = new ObjectId,
  userPk: ObjectId,
  blocked: ObjectId,
  createdAt: Instant,
  isNotIndexed: Option[Boolean] = None) {
  def userId: ObjectId = userPk
  def id: ObjectId = _id
}

class UserBlockMeta(name: String = "userblocks") extends RCcMetaExt[UserBlock, UserBlockMeta](name)
  with QueryById[UserBlockMeta] {

  val thisIsWhatWorks = ObjectIdTaggedField[User]
  val createdAt = InstantField
}
/**
 * Created by mar on 02/06/2017.
 */
class RccMeta212Spec extends FlatSpec with Matchers {

  "UserBlockMeta" should "be creatable" in {
    val UserBlock = new UserBlockMeta()
   // println(s"User block ${UserBlock}")
    assert(UserBlock != null)
  }

}
