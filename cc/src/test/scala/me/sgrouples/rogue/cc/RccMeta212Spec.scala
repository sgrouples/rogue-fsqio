package me.sgrouples.rogue.cc

import java.time.Instant

import me.sgrouples.rogue.cc.macros.*
import org.bson.types.ObjectId
import munit.FunSuite
trait QueryById[M] {
  requires: QueryFieldHelpers[M] & M =>

  val thisIsWhatFails = ObjectIdField("_id")

}

case class UserBlock(
    _id: ObjectId = new ObjectId,
    userPk: ObjectId,
    blocked: ObjectId,
    createdAt: Instant,
    isNotIndexed: Option[Boolean] = None
) derives MacroBsonFormat {
  def userId: ObjectId = userPk
  def id: ObjectId = _id
}

class UserBlockMeta(name: String = "userblocks")
    extends MCcMeta[UserBlock, UserBlockMeta](name)
    with QueryById[UserBlockMeta] {

  val thisIsWhatWorks = ObjectIdSubtypeField[User.Id]("thisIsWhatWorks")
  val createdAt = InstantField("createdAt")
}

/** Created by mar on 02/06/2017.
  */
class RccMeta212Spec extends FunSuite {

  test("UserBlockMeta should be creatable") {
    val UserBlock = new UserBlockMeta()
    // println(s"User block ${UserBlock}")
    assert(UserBlock != null)
  }

}
