package me.sgrouples.rogue.cc

import java.time.LocalDateTime
import java.util.regex.Pattern

import io.fsq.rogue._
import me.sgrouples.rogue.LongField
//import io.fsq.rogue.test.TrivialAsyncORMTests
import me.sgrouples.rogue.cc.CcRogue._
import org.bson.types.ObjectId
import org.junit.{After, Before, Test}
import org.specs2.matcher.JUnitMustMatchers

import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable}
import CcRogue._


class EndToEndBsonAsyncTest extends JUnitMustMatchers {
  import Metas._

  val atMost = 5 seconds

  def blk[T](t: Awaitable[T]): T = Await.result(t, atMost)

  def baseTestVenue(): Venue = Venue(
    _id = new ObjectId(),
    legId = 123L,
    userId = 456L,
    venuename = "test venue",
    mayor = 789L,
    mayor_count = 3L,
    closed = false,
    popularity = List(1L, 2L, 3L),
    categories = List(new ObjectId()),
    latlng = LatLong(40.73, -73.98),
    last_updated = LocalDateTime.now(),
    status = VenueStatus.open,
    claims = List(VenueClaimBson(uid = 1234L, status = ClaimStatus.pending),
      VenueClaimBson(uid = 5678L, status = ClaimStatus.approved)),
    lastClaim = VenueClaimBson(uid = 5678L, status = ClaimStatus.approved),
    tags = List("test tag1", "some tag")
  )





  def baseTestVenueClaim(vid: ObjectId): VenueClaim = {
    VenueClaim(vid, 123L, ClaimStatus.approved)
  }

      def baseTestTip(): Tip = {
        Tip(new ObjectId(), legid = 234L, counts = Map("foo" -> 1L, "bar" -> 2L))
      }


        @Before
      def setupMongoConnection: Unit = {
        val m = MongoTestConn.connectToMongo
        CcMongo.defineDb("default", m, "rogue-test-async")
      }

      @After
      def cleanupTestData: Unit = {

        blk(VenueR.bulkDeleteAsync_!!!())
        blk(VenueR.countAsync()) must_== 0

        blk(VenueClaimR.bulkDeleteAsync_!!!())
        blk(VenueClaimR.countAsync()) must_== 0

        //blk(Like.allShards.bulkDeleteAsync_!!!())

        MongoTestConn.disconnectFromMongo
      }

      @Test
      def eqsTests: Unit = {
        val v = baseTestVenue()
        blk(VenueR.insertOneAsync(v))
        val vc = baseTestVenueClaim(v._id)
        blk(VenueClaimR.insertOneAsync(vc))

        // eqs
        val q = ccMetaToQueryBuilder(VenueR).where(_.id eqs v._id)
        println(s"Ret class ${classOf[q.meta.R]}")
        blk(ccMetaToQueryBuilder(VenueR).where(_.id eqs v._id).fetchAsync()).map(_._id) must_== Seq(v._id)
        blk(VenueR.where(_.mayor eqs v.mayor).fetchAsync()).map(_._id) must_== List(v._id)
        blk(VenueR.where(_.mayor eqs v.mayor).fetchAsync()).map(_._id) must_== List(v._id)
        blk(VenueR.where(_.venuename eqs v.venuename).fetchAsync()).map(_._id) must_== List(v._id)
        blk(VenueR.where(_.closed eqs false).fetchAsync()).map(_._id) must_== List(v._id)

        blk(VenueR.where(_.mayor eqs 432432).fetchAsync()).map(_._id) must_== Nil
        blk(VenueR.where(_.closed eqs true).fetchAsync()).map(_._id) must_== Nil

        blk(VenueClaimR.where(_.status eqs ClaimStatus.approved).fetchAsync()).map(_._id) must_== List(vc._id)
        blk(VenueClaimR.where(_.venueid eqs v._id).fetchAsync()).map(_._id) must_== List(vc._id)
      }


  @Test
  def testInequalityQueries: Unit = {
    val v = baseTestVenue()
    blk(VenueR.insertOneAsync(v))
    val vc = baseTestVenueClaim(v._id)
    blk(VenueClaimR.insertOneAsync(vc))

    // neq,lt,gt, where the lone Venue has mayor_count=3, and the only
    // VenueClaim has status approved.
    blk(VenueR.where(_.mayor_count neqs 5).fetchAsync()).map(_._id) must_== List(v._id)
    blk(VenueR.where(_.mayor_count < 5).fetchAsync()).map(_._id) must_== List(v._id)
    blk(VenueR.where(_.mayor_count lt 5).fetchAsync()).map(_._id) must_== List(v._id)
    blk(VenueR.where(_.mayor_count <= 5).fetchAsync()).map(_._id) must_== List(v._id)
    blk(VenueR.where(_.mayor_count lte 5).fetchAsync()).map(_._id) must_== List(v._id)
    blk(VenueR.where(_.mayor_count > 5).fetchAsync()).map(_._id) must_== Nil
    blk(VenueR.where(_.mayor_count gt 5).fetchAsync()).map(_._id) must_== Nil
    blk(VenueR.where(_.mayor_count >= 5).fetchAsync()).map(_._id) must_== Nil
    blk(VenueR.where(_.mayor_count gte 5).fetchAsync()).map(_._id) must_== Nil
    blk(VenueR.where(_.mayor_count between(3, 5)).fetchAsync()).map(_._id) must_== List(v._id)
    blk(VenueClaimR.where(_.status neqs ClaimStatus.approved).fetchAsync()).map(_._id) must_== Nil
    blk(VenueClaimR.where(_.status neqs ClaimStatus.pending).fetchAsync()).map(_._id) must_== List(vc._id)
  }


  @Test
  def selectQueries: Unit = {
    val v = baseTestVenue()
    blk(VenueR.insertOneAsync(v))

    val base = VenueR.where(_.id eqs v._id)
    //val f = mandatoryFieldToSelectField(VenueR.legacyid)

    blk(base.select(_.legacyid).fetchAsync()) must_== List(v.legId)

    blk(base.select(_.legacyid, _.userid).fetchAsync()) must_== List((v.legId, v.userId))
    blk(base.select(_.legacyid, _.userid, _.mayor).fetchAsync()) must_== List((v.legId, v.userId, v.mayor))
    blk(base.select(_.legacyid, _.userid, _.mayor, _.mayor_count).fetchAsync()) must_== List((v.legId, v.userId, v.mayor, v.mayor_count))
    blk(base.select(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed).fetchAsync()) must_== List((v.legId, v.userId, v.mayor, v.mayor_count, v.closed))
    blk(base.select(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, _.tags).fetchAsync()) must_== List((v.legId, v.userId, v.mayor, v.mayor_count, v.closed, v.tags))
  }

  @Test
  def selectEnum: Unit = {
    val v = baseTestVenue()
    blk(VenueR.insertOneAsync(v))
    blk(VenueR.where(_.id eqs v._id).select(_.status).fetchAsync()) must_== List(VenueStatus.open)
  }

  @Test
  def selectCaseQueries: Unit = {
    val v = baseTestVenue()
    blk(VenueR.insertOneAsync(v))

    val base = VenueR.where(_.id eqs v._id)
    blk(base.selectCase(_.legacyid, V1).fetchAsync()) must_== List(V1(v.legId))
    blk(base.selectCase(_.legacyid, _.userid, V2).fetchAsync()) must_== List(V2(v.legId, v.userId))
    blk(base.selectCase(_.legacyid, _.userid, _.mayor, V3).fetchAsync()) must_== List(V3(v.legId, v.userId, v.mayor))
    blk(base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, V4).fetchAsync()) must_== List(V4(v.legId, v.userId, v.mayor, v.mayor_count))
    blk(base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, V5).fetchAsync()) must_== List(V5(v.legId, v.userId, v.mayor, v.mayor_count, v.closed))
    blk(base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, _.tags, V6).fetchAsync()) must_== List(V6(v.legId, v.userId, v.mayor, v.mayor_count, v.closed, v.tags))
  }


  @Test
  def selectSubfieldQueries: Unit = {
    val v = baseTestVenue()
    blk(VenueR.insertOneAsync(v))
    val t = baseTestTip()
    blk(TipR.insertOneAsync(t))

    //TODO - no support for querying map fields now
    // select subfields
    //val q=TipR.where(_.id eqs t._id).select(_.counts at "foo")
    //println(s"Q ${q.query}")
    //blk(q.fetchAsync()) must_== Seq(Some(1L))

    //todo - no unsafe fields now
    //blk(VenueR.where(_.id eqs v._id).select(_.geolatlng.unsafeField[Double]("lat")).fetchAsync()) must_== List(Some(40.73))
    val subuserids: Seq[Option[List[Long]]] = blk(VenueR.where(_.id eqs v._id).select(_.claims.subselect(_.uid)).fetchAsync())
    println(s"Sub user ids ${subuserids}")
    subuserids must_== List(Some(List(1234, 5678)))

    /*val q = VenueR.where(_.claims.subfield(_.uid) eqs 1234).select(_.claims.$$)
    val subclaims: Seq[List[VenueClaimBson]] = blk(q.fetchAsync())
    subclaims.size must_== 1
    subclaims.head.size must_== 1
    subclaims.head.head.uid must_== 1234
    subclaims.head.head.status must_== ClaimStatus.pending
*/
    // selecting a claims.userid when there is no top-level claims list should
    // have one element in the List for the one Venue, but an Empty for that
    // Venue since there's no list of claims there.
    blk(VenueR.where(_.id eqs v._id).modify(_.claims unset).and(_.lastClaim unset).updateOneAsync())
    //val x =caseClassFieldToQueryField(VenueR.lastClaim).subfield(_.uid)

    //val d = VenueR.select(_.lastClaim.subfield(_.uid))
    //val f= roptionalFieldToSelectField(ccMetaToQueryBuilder(VenueR).select(_.lastClaim.subfield(_.uid)))
    //val q = VenueR.where(_.id eqs v._id).select(_.lastClaim.subfield(_.uid))

    blk(VenueR.where(_.id eqs v._id).select(_.lastClaim.subfield(_.uid)).fetchAsync()) must_== List(None)
    blk(VenueR.where(_.id eqs v._id).select(_.lastClaim.subselect(_.uid)).fetchAsync()) must_== List(None)
    blk(VenueR.where(_.id eqs v._id).select(_.claims.subselect(_.uid)).fetchAsync()) must_== List(None)
  }
/*
  @Ignore("These tests are broken because DummyField doesn't know how to convert a String to an Enum")
  def testSelectEnumSubfield: Unit = {
        val v = baseTestVenue()
        blk(VenueR.insertOneAsync(v))

    // This behavior is broken because we get a String back from mongo, and at
    // that point we only have a DummyField for the subfield, and that doesn't
    // know how to convert the String to an Enum.

    val statuses: Seq[Option[VenueClaimBson.status.MyType]] =
      blk(VenueR.where(_._id eqs v._id).select(_.lastClaim.subselect(_.status)).fetchAsync())
    // This assertion works.
    statuses must_== List(Some("Approved"))
    // This assertion is what we want, and it fails.
    // statuses must_== List(Some(ClaimStatus.approved))

    val subuseridsAndStatuses: Seq[(Option[List[Long]], Option[List[VenueClaimBson.status.MyType]])] =
      blk(VenueR.where(_._id eqs v._id)
        .select(_.claims.subselect(_.userid), _.claims.subselect(_.status))
        .fetchAsync())
    // This assertion works.
    subuseridsAndStatuses must_== Seq((Some(List(1234, 5678)), Some(List("Pending approval", "Approved"))))

    // This assertion is what we want, and it fails.
    // subuseridsAndStatuses must_== List((Some(List(1234, 5678)), Some(List(ClaimStatus.pending, ClaimStatus.approved))))
  }

  @Test
  def testReadPreference: Unit = {
    // Note: this isn't a real test of readpreference because the test mongo setup
    // doesn't have replicas. This basically just makes sure that readpreference
    // doesn't break everything.
        val v = baseTestVenue()
        blk(VenueR.insertOneAsync(v))

    // eqs
    blk(VenueR.where(_._id eqs v._id).fetchAsync()).map(_._id) must_== Seq(v._id)
    blk(VenueR.where(_._id eqs v._id).setReadPreference(ReadPreference.secondary).fetchAsync()).map(_._id) must_== Seq(v._id)
    blk(VenueR.where(_._id eqs v._id).setReadPreference(ReadPreference.primary).fetchAsync()).map(_._id) must_== Seq(v._id)
  }
  @Test
  def testFindAndModify {
    val v1 = blk(VenueR.where(_.venuename eqs "v1")
      .findAndModify(_.userid setTo 5)
      .upsertOneAsync(returnNew = false))
    v1 must_== None

    val v2 = blk(VenueR.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 5)
      .upsertOneAsync(returnNew = true))
    v2.map(_.userid) must_== Some(5)

    val v3 = blk(VenueR.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 6)
      .upsertOneAsync(returnNew = false))
    v3.map(_.userid) must_== Some(5)

    val v4 = blk(VenueR.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 7)
      .upsertOneAsync(returnNew = true))
    v4.map(_.userid) must_== Some(7)
  }
  @Test
  def testRegexQuery {
        val v = baseTestVenue()
        blk(VenueR.insertOneAsync(v))
    blk(VenueR.where(_.id eqs v._id).and(_.venuename startsWith "test v").countAsync()) must_== 1
    blk(VenueR.where(_.id eqs v._id).and(_.venuename matches ".es. v".r).countAsync()) must_== 1
    blk(VenueR.where(_.id eqs v._id).and(_.venuename matches "Tes. v".r).countAsync()) must_== 0
    blk(VenueR.where(_.id eqs v._id).and(_.venuename matches Pattern.compile("Tes. v", Pattern.CASE_INSENSITIVE)).countAsync()) must_== 1
    blk(VenueR.where(_.id eqs v._id).and(_.venuename matches "test .*".r).and(_.legacyid in List(v.legId)).countAsync()) must_== 1
    blk(VenueR.where(_.id eqs v._id).and(_.venuename matches "test .*".r).and(_.legacyid nin List(v.legId)).countAsync()) must_== 0
    blk(VenueR.where(_.tags matches """some\s.*""".r).countAsync()) must_== 1
  }
*/

  @Test
  def testLimitAndBatch {
    (1 to 50).foreach(_ => blk(VenueR.insertOneAsync(baseTestVenue())))
    val q = VenueR.select(_.id)
    blk(q.limit(10).fetchAsync()).length must_== 10
    blk(q.limit(-10).fetchAsync()).length must_== 10
  }
  @Test
  def testCount {
    (1 to 10).foreach(_ => blk(VenueR.insertOneAsync(baseTestVenue())))
    val q = VenueR.select(_.id)
    blk(q.countAsync()) must_== 10
    blk(q.limit(3).countAsync()) must_== 3
    blk(q.limit(15).countAsync()) must_== 10
    blk(q.skip(5).countAsync()) must_== 5
    blk(q.skip(12).countAsync()) must_== 0
    blk(q.skip(3).limit(5).countAsync()) must_== 5
    blk(q.skip(8).limit(4).countAsync()) must_== 2
  }
/* distincts need codecs for Long, and probably Int in db
  @Test
  def testDistinct {
    (1 to 5).foreach(_ => blk(VenueR.insertOneAsync(baseTestVenue().copy(userId = 1L))))
    (1 to 5).foreach(_ => blk(VenueR.insertOneAsync(baseTestVenue().copy(userId = 2L))))
    (1 to 5).foreach(_ => blk(VenueR.insertOneAsync(baseTestVenue().copy(userId = 3L))))
    blk(VenueR.where(_.mayor eqs 789L).distinctAsync(_.userid)).length must_== 3
    blk(VenueR.where(_.mayor eqs 789L).countDistinctAsync(_.userid)) must_== 3
  }
*/

  @Test
  def testSlice():Unit = {
    val v= baseTestVenue().copy(tags = List("1", "2", "3", "4"))
    blk(VenueR.insertOneAsync(v))
    blk(VenueR.select(_.tags.slice(2)).getAsync()) must_== Some(List("1", "2"))
    blk(VenueR.select(_.tags.slice(-2)).getAsync()) must_== Some(List("3", "4"))
    blk(VenueR.select(_.tags.slice(1, 2)).getAsync()) must_== Some(List("2", "3"))
  }

}

