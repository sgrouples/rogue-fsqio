// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.
package io.fsq.rogue.lift.test

import java.util.regex.Pattern

import com.mongodb.ReadPreference
import io.fsq.rogue._
import io.fsq.rogue.lift.LiftRogue._
import org.bson.types.ObjectId
import org.scalatest.concurrent.ScalaFutures
import org.scalatest._

/**
 * Contains tests that test the interaction of Rogue with a real mongo.
 */
class EndToEndAsyncSpec extends FlatSpec
    with Matchers with ScalaFutures
    with BeforeAndAfterEach {

  def baseTestVenue(): Venue = {
    Venue.createRecord
      .legacyid(123)
      .userid(456)
      .venuename("test venue")
      .mayor(789)
      .mayor_count(3)
      .closed(false)
      .popularity(List(1L, 2L, 3L))
      .categories(List(new ObjectId()))
      .geolatlng(LatLong(40.73, -73.98))
      .status(VenueStatus.open)
      .claims(List(
        VenueClaimBson.createRecord.userid(1234).status(ClaimStatus.pending),
        VenueClaimBson.createRecord.userid(5678).status(ClaimStatus.approved)
      ))
      .lastClaim(VenueClaimBson.createRecord.userid(5678).status(ClaimStatus.approved))
      .tags(List("test tag1", "some tag"))
  }

  def baseTestVenueClaim(vid: ObjectId): VenueClaim = {
    VenueClaim.createRecord
      .venueid(vid)
      .userid(123)
      .status(ClaimStatus.approved)
  }

  def baseTestTip(): Tip = {
    Tip.createRecord
      .legacyid(234)
      .counts(Map(
        "foo" -> 1L,
        "bar" -> 2L
      ))
  }

  override protected def beforeEach(): Unit = {
    RogueTestMongo.connectToMongoAsync
    RogueTestMongo.connectToMongo
  }

  override protected def afterEach(): Unit = {
    Venue.bulkDeleteAsync_!!!().futureValue
    Venue.countAsync().futureValue shouldBe 0

    VenueClaim.bulkDeleteAsync_!!!().futureValue
    VenueClaim.countAsync().futureValue shouldBe 0

    Like.allShards.bulkDeleteAsync_!!!().futureValue

    RogueTestMongo.disconnectFromMongoAsync
    RogueTestMongo.disconnectFromMongo
  }

  "eqs" should "work as expected" in {
    val v = baseTestVenue()
    v.insertAsync()
    val vc = baseTestVenueClaim(v.id)
    vc.insertAsync().futureValue

    // eqs
    metaRecordToQueryBuilder(Venue).where(_._id eqs v.id).fetchAsync().futureValue.map(_.id) shouldBe Seq(v.id)
    Venue.where(_.mayor eqs v.mayor.value).fetchAsync().futureValue.map(_.id) shouldBe List(v.id)
    Venue.where(_.mayor eqs v.mayor.value).fetchAsync().futureValue.map(_.id) shouldBe List(v.id)
    Venue.where(_.venuename eqs v.venuename.value).fetchAsync().futureValue.map(_.id) shouldBe List(v.id)
    Venue.where(_.closed eqs false).fetchAsync().futureValue.map(_.id) shouldBe List(v.id)

    Venue.where(_.mayor eqs 432432).fetchAsync().futureValue.map(_.id) shouldBe Nil
    Venue.where(_.closed eqs true).fetchAsync().futureValue.map(_.id) shouldBe Nil

    VenueClaim.where(_.status eqs ClaimStatus.approved).fetchAsync().futureValue.map(_.id) shouldBe List(vc.id)
    VenueClaim.where(_.venueid eqs v.id).fetchAsync().futureValue.map(_.id) shouldBe List(vc.id)
    VenueClaim.where(_.venueid eqs v).fetchAsync().futureValue.map(_.id) shouldBe List(vc.id)
  }

  "inequality queries" should "work as expected" in {
    val v = baseTestVenue()
    v.insertAsync()
    val vc = baseTestVenueClaim(v.id)
    vc.insertAsync()

    // neq,lt,gt, where the lone Venue has mayor_count=3, and the only
    // VenueClaim has status approved.
    Venue.where(_.mayor_count neqs 5).fetchAsync().futureValue.map(_.id) shouldBe List(v.id)
    Venue.where(_.mayor_count < 5).fetchAsync().futureValue.map(_.id) shouldBe List(v.id)
    Venue.where(_.mayor_count lt 5).fetchAsync().futureValue.map(_.id) shouldBe List(v.id)
    Venue.where(_.mayor_count <= 5).fetchAsync().futureValue.map(_.id) shouldBe List(v.id)
    Venue.where(_.mayor_count lte 5).fetchAsync().futureValue.map(_.id) shouldBe List(v.id)
    Venue.where(_.mayor_count > 5).fetchAsync().futureValue.map(_.id) shouldBe Nil
    Venue.where(_.mayor_count gt 5).fetchAsync().futureValue.map(_.id) shouldBe Nil
    Venue.where(_.mayor_count >= 5).fetchAsync().futureValue.map(_.id) shouldBe Nil
    Venue.where(_.mayor_count gte 5).fetchAsync().futureValue.map(_.id) shouldBe Nil
    Venue.where(_.mayor_count between (3, 5)).fetchAsync().futureValue.map(_.id) shouldBe List(v.id)
    VenueClaim.where(_.status neqs ClaimStatus.approved).fetchAsync().futureValue.map(_.id) shouldBe Nil
    VenueClaim.where(_.status neqs ClaimStatus.pending).fetchAsync().futureValue.map(_.id) shouldBe List(vc.id)
  }

  "select queries" should "work as expected" in {
    val v = baseTestVenue()
    v.insertAsync()

    val base = Venue.where(_._id eqs v.id)
    base.select(_.legacyid).fetchAsync().futureValue shouldBe List(v.legacyid.value)
    base.select(_.legacyid, _.userid).fetchAsync().futureValue shouldBe List((v.legacyid.value, v.userid.value))
    base.select(_.legacyid, _.userid, _.mayor).fetchAsync().futureValue shouldBe List((v.legacyid.value, v.userid.value, v.mayor.value))
    base.select(_.legacyid, _.userid, _.mayor, _.mayor_count).fetchAsync().futureValue shouldBe List((v.legacyid.value, v.userid.value, v.mayor.value, v.mayor_count.value))
    base.select(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed).fetchAsync().futureValue shouldBe List((v.legacyid.value, v.userid.value, v.mayor.value, v.mayor_count.value, v.closed.value))
    base.select(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, _.tags).fetchAsync().futureValue shouldBe List((v.legacyid.value, v.userid.value, v.mayor.value, v.mayor_count.value, v.closed.value, v.tags.value))
  }

  "selecting enum" should "work as expected" in {
    val v = baseTestVenue()
    v.insertAsync()
    Venue.where(_._id eqs v.id).select(_.status).fetchAsync().futureValue shouldBe List(VenueStatus.open)
  }

  "selecting case class" should "work as expected" in {
    val v = baseTestVenue()
    v.insertAsync()

    val base = Venue.where(_._id eqs v.id)
    base.selectCase(_.legacyid, V1).fetchAsync().futureValue shouldBe List(V1(v.legacyid.value))
    base.selectCase(_.legacyid, _.userid, V2).fetchAsync().futureValue shouldBe List(V2(v.legacyid.value, v.userid.value))
    base.selectCase(_.legacyid, _.userid, _.mayor, V3).fetchAsync().futureValue shouldBe List(V3(v.legacyid.value, v.userid.value, v.mayor.value))
    base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, V4).fetchAsync().futureValue shouldBe List(V4(v.legacyid.value, v.userid.value, v.mayor.value, v.mayor_count.value))
    base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, V5).fetchAsync().futureValue shouldBe List(V5(v.legacyid.value, v.userid.value, v.mayor.value, v.mayor_count.value, v.closed.value))
    base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, _.tags, V6).fetchAsync().futureValue shouldBe List(V6(v.legacyid.value, v.userid.value, v.mayor.value, v.mayor_count.value, v.closed.value, v.tags.value))
  }

  "sub-field queries" should "work as expected" in {
    val v = baseTestVenue()
    v.insertAsync()
    val t = baseTestTip()
    t.insertAsync()

    // select subfields
    Tip.where(_._id eqs t.id).select(_.counts at "foo").fetchAsync().futureValue shouldBe Seq(Some(1L))
    Venue.where(_._id eqs v.id).select(_.geolatlng.unsafeField[Double]("lat")).fetchAsync().futureValue shouldBe List(Some(40.73))

    val subuserids: Seq[Option[List[Long]]] = {
      Venue.where(_._id eqs v.id)
        .select(_.claims.subselect(_.userid))
        .fetchAsync().futureValue
    }

    subuserids shouldBe List(Some(List(1234, 5678)))

    val q = Venue.where(_.claims.subfield(_.userid) eqs 1234).select(_.claims.$$)
    val subclaims: Seq[List[VenueClaimBson]] = q.fetchAsync().futureValue
    subclaims.size shouldBe 1
    subclaims.head.size shouldBe 1
    subclaims.head.head.userid.value shouldBe 1234
    subclaims.head.head.status.value shouldBe ClaimStatus.pending

    // selecting a claims.userid when there is no top-level claims list should
    // have one element in the List for the one Venue, but an Empty for that
    // Venue since there's no list of claims there.
    Venue.where(_._id eqs v.id).modify(_.claims unset).and(_.lastClaim unset).updateOneAsync()
    Venue.where(_._id eqs v.id).select(_.lastClaim.subselect(_.userid)).fetchAsync().futureValue shouldBe List(None)

    val q1 = Venue.where(_._id eqs v.id).select(_.lastClaim.subselect(_.userid))
    val q2 = queryToLiftQuery(Venue.select(_.lastClaim.subselect(_.userid)))
    //println(s"QQ ${q2}")

    Venue.where(_._id eqs v.id).select(_.claims.subselect(_.userid)).fetchAsync().futureValue shouldBe List(None)
  }

  // "These tests are broken because DummyField doesn't know how to convert a String to an Enum"
  "selecting enum sub-field" should "work as expected" ignore {
    val v = baseTestVenue()
    v.insertAsync()

    // This behavior is broken because we get a String back from mongo, and at
    // that point we only have a DummyField for the subfield, and that doesn't
    // know how to convert the String to an Enum.

    val statuses: Seq[Option[VenueClaimBson.status.MyType]] =
      Venue.where(_._id eqs v.id).select(_.lastClaim.subselect(_.status)).fetchAsync().futureValue
    // This assertion works.
    statuses shouldBe List(Some("Approved"))
    // This assertion is what we want, and it fails.
    // statuses shouldBe List(Some(ClaimStatus.approved))

    val subuseridsAndStatuses: Seq[(Option[List[Long]], Option[List[VenueClaimBson.status.MyType]])] =
      Venue.where(_._id eqs v.id)
        .select(_.claims.subselect(_.userid), _.claims.subselect(_.status))
        .fetchAsync().futureValue
    // This assertion works.
    subuseridsAndStatuses shouldBe Seq((Some(List(1234, 5678)), Some(List("Pending approval", "Approved"))))

    // This assertion is what we want, and it fails.
    // subuseridsAndStatuses shouldBe List((Some(List(1234, 5678)), Some(List(ClaimStatus.pending, ClaimStatus.approved))))
  }

  "read preferences" should "work as expected" in {
    // Note: this isn't a real test of readpreference because the test mongo setup
    // doesn't have replicas. This basically just makes sure that readpreference
    // doesn't break everything.
    val v = baseTestVenue()
    v.insertAsync().futureValue

    // eqs
    Venue.where(_._id eqs v.id).fetchAsync().futureValue.map(_.id) shouldBe Seq(v.id)
    Venue.where(_._id eqs v.id).setReadPreference(ReadPreference.secondary).fetchAsync().futureValue.map(_.id) shouldBe Seq(v.id)
    Venue.where(_._id eqs v.id).setReadPreference(ReadPreference.primary).fetchAsync().futureValue.map(_.id) shouldBe Seq(v.id)
  }

  "find and modify" should "work as expected" in {
    val v1 = Venue.where(_.venuename eqs "v1")
      .findAndModify(_.userid setTo 5)
      .upsertOneAsync(returnNew = false)
      .futureValue
    v1 shouldBe None

    val v2 = Venue.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 5)
      .upsertOneAsync(returnNew = true)
      .futureValue
    v2.map(_.userid.value) shouldBe Some(5)

    val v3 = Venue.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 6)
      .upsertOneAsync(returnNew = false)
      .futureValue
    v3.map(_.userid.value) shouldBe Some(5)

    val v4 = Venue.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 7)
      .upsertOneAsync(returnNew = true)
      .futureValue
    v4.map(_.userid.value) shouldBe Some(7)
  }

  "Regex query" should "work as expected" in {
    val v = baseTestVenue()
    v.insertAsync()
    Venue.where(_._id eqs v.id).and(_.venuename startsWith "test v").countAsync().futureValue shouldBe 1
    Venue.where(_._id eqs v.id).and(_.venuename matches ".es. v".r).countAsync().futureValue shouldBe 1
    Venue.where(_._id eqs v.id).and(_.venuename matches "Tes. v".r).countAsync().futureValue shouldBe 0
    Venue.where(_._id eqs v.id).and(_.venuename matches Pattern.compile("Tes. v", Pattern.CASE_INSENSITIVE)).countAsync().futureValue shouldBe 1
    Venue.where(_._id eqs v.id).and(_.venuename matches "test .*".r).and(_.legacyid in List(v.legacyid.value)).countAsync().futureValue shouldBe 1
    Venue.where(_._id eqs v.id).and(_.venuename matches "test .*".r).and(_.legacyid nin List(v.legacyid.value)).countAsync().futureValue shouldBe 0
    Venue.where(_.tags matches """some\s.*""".r).countAsync().futureValue shouldBe 1
  }

  "Batch and limit" should "work as expected" in {
    (1 to 50).foreach(_ => baseTestVenue().insertAsync().futureValue)
    val q = Venue.select(_._id)
    q.limit(10).fetchAsync().futureValue.length shouldBe 10
    q.limit(-10).fetchAsync().futureValue.length shouldBe 10
  }

  "count" should "work as expected" in {
    (1 to 10).foreach(_ => baseTestVenue().insertAsync().futureValue)
    val q = Venue.select(_._id)
    q.countAsync().futureValue shouldBe 10
    q.limit(3).countAsync().futureValue shouldBe 3
    q.limit(15).countAsync().futureValue shouldBe 10
    q.skip(5).countAsync().futureValue shouldBe 5
    q.skip(12).countAsync().futureValue shouldBe 0
    q.skip(3).limit(5).countAsync().futureValue shouldBe 5
    q.skip(8).limit(4).countAsync().futureValue shouldBe 2
  }

  "distinct" should "work as expected" in {
    (1 to 5).foreach(_ => baseTestVenue().userid(1).insertAsync())
    (1 to 5).foreach(_ => baseTestVenue().userid(2).insertAsync())
    (1 to 5).foreach(_ => baseTestVenue().userid(3).insertAsync())
    Venue.where(_.mayor eqs 789).distinctAsync(_.userid).futureValue.length shouldBe 3
    Venue.where(_.mayor eqs 789).countDistinctAsync(_.userid).futureValue shouldBe 3
  }

  "slice" should "work as expected" in {
    baseTestVenue().tags(List("1", "2", "3", "4")).insertAsync().futureValue
    Venue.select(_.tags.slice(2)).getAsync().futureValue shouldBe Some(List("1", "2"))
    Venue.select(_.tags.slice(-2)).getAsync().futureValue shouldBe Some(List("3", "4"))
    Venue.select(_.tags.slice(1, 2)).getAsync().futureValue shouldBe Some(List("2", "3"))
  }

  "Map field" should "work as expected" in {

    val emojiCount = Map(":smiley:" -> 2L)
    baseTestVenue().emojiCounts(emojiCount).insertAsync().futureValue
    Venue.select(_.emojiCounts).getAsync.futureValue shouldBe Some(emojiCount)
    Venue.where(_.mayor eqs 789).getAsync.futureValue.map(_.emojiCounts.get) shouldBe Some(emojiCount)
  }
}
