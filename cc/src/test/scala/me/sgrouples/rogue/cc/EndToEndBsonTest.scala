package me.sgrouples.rogue.cc

import java.time.LocalDateTime
import java.util.regex.Pattern

import com.mongodb.client.MongoDatabase
import io.fsq.rogue._
import me.sgrouples.rogue.LongField
//import io.fsq.rogue.test.TrivialORMTests
import me.sgrouples.rogue.cc.CcRogue._
import org.bson.types.ObjectId
import org.junit.{ After, Before, Test }
import org.specs2.matcher.JUnitMustMatchers

import scala.concurrent.duration._
import scala.concurrent.{ Await, Awaitable }
import CcRogue._

class EndToEndBsonTest extends JUnitMustMatchers {
  import Metas._

  val lastClaim = VenueClaimBson(uid = 5678L, status = ClaimStatus.approved)
  val firstClaim = VenueClaimBson(uid = 5679L, status = ClaimStatus.approved)

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
    last_updated = LocalDateTime.now(),
    status = VenueStatus.open,
    claims = List(
      VenueClaimBson(uid = 1234L, status = ClaimStatus.pending),
      VenueClaimBson(uid = 5678L, status = ClaimStatus.approved)
    ),
    lastClaim = Option(lastClaim),
    tags = List("test tag1", "some tag")
  )

  def baseTestVenueClaim(vid: ObjectId): VenueClaim = {
    VenueClaim(new ObjectId(), vid, 123L, ClaimStatus.approved)
  }

  def baseTestTip(): Tip = {
    Tip(new ObjectId(), legid = 234L, counts = Map("foo" -> 1L, "bar" -> 2L))
  }

  private var dbOpt: Option[MongoDatabase] = None
  implicit def db = dbOpt.getOrElse(throw new UninitializedError)

  @Before
  def setupMongoConnection: Unit = {
    val m = MongoTestConn.connectToMongoSync
    CcMongo.defineDbSync("default", m, "rogue-test-")
    CcMongo.defineDbSync("lift", m, "rogue-test-")
    dbOpt = CcMongo.getDbSync("lift")
  }

  @After
  def cleanupTestData: Unit = {

    VenueR.bulkDelete_!!!()
    VenueR.count() must_== 0

    VenueClaimR.bulkDelete_!!!()
    VenueClaimR.count() must_== 0

    OptValCCR.bulkDelete_!!!()
    //Like.allShards.bulkDelete_!!!()

    MongoTestConn.disconnectFromMongoSync
  }

  @Test
  def eqsTests: Unit = {
    val v = baseTestVenue()
    VenueR.insertOne(v)
    val vc = baseTestVenueClaim(v._id)
    VenueClaimR.insertOne(vc)

    // eqs
    val q = ccMetaToQueryBuilder(VenueR).where(_.id eqs v._id)
    //println(s"Ret class ${classOf[q.meta.R]}")
    ccMetaToQueryBuilder(VenueR).where(_.id eqs v._id).fetch().map(_._id) must_== Seq(v._id)
    VenueR.where(_.mayor eqs v.mayor).fetch().map(_._id) must_== List(v._id)
    VenueR.where(_.mayor eqs v.mayor).fetch().map(_._id) must_== List(v._id)
    VenueR.where(_.venuename eqs v.venuename).fetch().map(_._id) must_== List(v._id)
    VenueR.where(_.closed eqs false).fetch().map(_._id) must_== List(v._id)

    VenueR.where(_.mayor eqs 432432).fetch().map(_._id) must_== Nil
    VenueR.where(_.closed eqs true).fetch().map(_._id) must_== Nil

    VenueClaimR.fetch().map(println)
    VenueClaimR.where(_.status eqs ClaimStatus.approved).fetch().map(_._id) must_== List(vc._id)
    VenueClaimR.where(_.venueid eqs v._id).fetch().map(_._id) must_== List(vc._id)
  }

  @Test
  def testInequalityQueries: Unit = {
    val v = baseTestVenue()
    VenueR.insertOne(v)
    val vc = baseTestVenueClaim(v._id)
    VenueClaimR.insertOne(vc)

    // neq,lt,gt, where the lone Venue has mayor_count=3, and the only
    // VenueClaim has status approved.
    val h = VenueR.where(_.mayor_count neqs 5).fetch()
    VenueR.where(_.mayor_count neqs 5).fetch().map(_._id) must_== List(v._id)
    VenueR.where(_.mayor_count < 5).fetch().map(_._id) must_== List(v._id)
    VenueR.where(_.mayor_count lt 5).fetch().map(_._id) must_== List(v._id)
    VenueR.where(_.mayor_count <= 5).fetch().map(_._id) must_== List(v._id)
    VenueR.where(_.mayor_count lte 5).fetch().map(_._id) must_== List(v._id)
    VenueR.where(_.mayor_count > 5).fetch().map(_._id) must_== Nil
    VenueR.where(_.mayor_count gt 5).fetch().map(_._id) must_== Nil
    VenueR.where(_.mayor_count >= 5).fetch().map(_._id) must_== Nil
    VenueR.where(_.mayor_count gte 5).fetch().map(_._id) must_== Nil
    VenueR.where(_.mayor_count between (3, 5)).fetch().map(_._id) must_== List(v._id)
    VenueClaimR.where(_.status neqs ClaimStatus.approved).fetch().map(_._id) must_== Nil
    VenueClaimR.where(_.status neqs ClaimStatus.pending).fetch().map(_._id) must_== List(vc._id)
  }

  @Test
  def selectQueries: Unit = {
    val v = baseTestVenue()
    VenueR.insertOne(v)

    val base = VenueR.where(_.id eqs v._id)
    //val f = mandatoryFieldToSelectField(VenueR.legacyid)

    base.select(_.legacyid).fetch() must_== List(v.legId)

    val x = base.select(_.legacyid, _.userid).fetch() must_== List((v.legId, v.userId))
    base.select(_.legacyid, _.userid, _.mayor).fetch() must_== List((v.legId, v.userId, v.mayor))
    base.select(_.legacyid, _.userid, _.mayor, _.mayor_count).fetch() must_== List((v.legId, v.userId, v.mayor, v.mayor_count))
    base.select(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed).fetch() must_== List((v.legId, v.userId, v.mayor, v.mayor_count, v.closed))
    base.select(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, _.tags).fetch() must_== List((v.legId, v.userId, v.mayor, v.mayor_count, v.closed, v.tags))
  }

  @Test
  def selectEnum: Unit = {
    val v = baseTestVenue()
    VenueR.insertOne(v)
    VenueR.where(_.id eqs v._id).select(_.status).fetch() must_== List(VenueStatus.open)
  }

  @Test
  def selectCaseQueries: Unit = {
    val v = baseTestVenue()
    VenueR.insertOne(v)

    val base = VenueR.where(_.id eqs v._id)
    base.selectCase(_.legacyid, V1).fetch() must_== List(V1(v.legId))
    base.selectCase(_.legacyid, _.userid, V2).fetch() must_== List(V2(v.legId, v.userId))
    base.selectCase(_.legacyid, _.userid, _.mayor, V3).fetch() must_== List(V3(v.legId, v.userId, v.mayor))
    base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, V4).fetch() must_== List(V4(v.legId, v.userId, v.mayor, v.mayor_count))
    base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, V5).fetch() must_== List(V5(v.legId, v.userId, v.mayor, v.mayor_count, v.closed))
    base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, _.tags, V6).fetch() must_== List(V6(v.legId, v.userId, v.mayor, v.mayor_count, v.closed, v.tags))
  }

  @Test
  def selectSubfieldQueries: Unit = {
    val v = baseTestVenue()
    VenueR.insertOne(v)
    val t = baseTestTip()
    TipR.insertOne(t)

    //TODO - no support for querying map fields now
    // select subfields
    val q3 = TipR.where(_.id eqs t._id).select(_.counts at "foo")
    //println(s"Q ${q.query}")
    q3.fetch() must_== Seq(Some(1L))

    //todo - no unsafe fields now
    //VenueR.where(_.id eqs v._id).select(_.geolatlng.unsafeField[Double]("lat").fetch() must_== List(Some(40.73)
    val subuserids: Seq[Option[List[Long]]] = VenueR.where(_.id eqs v._id).select(_.claims.subselect(_.uid)).fetch()
    //println(s"Sub user ids ${subuserids}")
    subuserids must_== List(Some(List(1234, 5678)))

    val q = VenueR.where(_.claims.subfield(_.uid) eqs 1234).select(_.claims.$$)
    val subclaims: Seq[Seq[VenueClaimBson]] = q.fetch()
    subclaims.size must_== 1
    subclaims.head.size must_== 1
    subclaims.head.head.uid must_== 1234
    subclaims.head.head.status must_== ClaimStatus.pending

    // selecting a claims.userid when there is no top-level claims list should
    // have one element in the List for the one Venue, but an Empty for that
    // Venue since there's no list of claims there.
    VenueR.where(_.id eqs v._id).modify(_.claims unset).and(_.lastClaim unset).updateOne()
    //val x =caseClassFieldToQueryField(VenueR.lastClaim).subfield(_.uid)

    //val d = VenueR.select(_.lastClaim.subfield(_.uid)
    //val f= roptionalFieldToSelectField(ccMetaToQueryBuilder(VenueR).select(_.lastClaim.subfield(_.uid))
    //val q = VenueR.where(_.id eqs v._id).select(_.lastClaim.subfield(_.uid)

    VenueR.where(_.id eqs v._id).select(_.lastClaim.subfield(_.uid)).fetch() must_== List(None)
    VenueR.where(_.id eqs v._id).select(_.lastClaim.subselect(_.uid)).fetch() must_== List(None)
    VenueR.where(_.id eqs v._id).select(_.claims.subselect(_.uid)).fetch() must_== List(None)
  }
  /*
    @Ignore("These tests are broken because DummyField doesn't know how to convert a String to an Enum")
    def testSelectEnumSubfield: Unit = {
          val v = baseTestVenue()
          VenueR.insertOne(v)

      // This behavior is broken because we get a String back from mongo, and at
      // that point we only have a DummyField for the subfield, and that doesn't
      // know how to convert the String to an Enum.

      val statuses: Seq[Option[VenueClaimBson.status.MyType]] =
        VenueR.where(_._id eqs v._id).select(_.lastClaim.subselect(_.status).fetch()
      // This assertion works.
      statuses must_== List(Some("Approved")
      // This assertion is what we want, and it fails.
      // statuses must_== List(Some(ClaimStatus.approved)

      val subuseridsAndStatuses: Seq[(Option[List[Long]], Option[List[VenueClaimBson.status.MyType]])] =
        VenueR.where(_._id eqs v._id)
          .select(_.claims.subselect(_.userid), _.claims.subselect(_.status)
          .fetch()
      // This assertion works.
      subuseridsAndStatuses must_== Seq((Some(List(1234, 5678), Some(List("Pending approval", "Approved"))

      // This assertion is what we want, and it fails.
      // subuseridsAndStatuses must_== List((Some(List(1234, 5678), Some(List(ClaimStatus.pending, ClaimStatus.approved))
    }

    @Test
    def testReadPreference: Unit = {
      // Note: this isn't a real test of readpreference because the test mongo setup
      // doesn't have replicas. This basically just makes sure that readpreference
      // doesn't break everything.
          val v = baseTestVenue()
          VenueR.insertOne(v)

      // eqs
      VenueR.where(_._id eqs v._id).fetch().map(_._id) must_== Seq(v._id)
      VenueR.where(_._id eqs v._id).setReadPreference(ReadPreference.secondary).fetch().map(_._id) must_== Seq(v._id)
      VenueR.where(_._id eqs v._id).setReadPreference(ReadPreference.primary).fetch().map(_._id) must_== Seq(v._id)
    }
  */

  /* Orignal test was broken - Veny has lots more required parameters than just userId
  case class Venue(_id: ObjectId, legId: Long, userId: Long, venuename: String, mayor: Long, mayor_count: Long, closed: Boolean, tags: List[String],
                 popularity: List[Long], categories: List[ObjectId], latlng: LatLong, last_updated: LocalDateTime, status: VenueStatus.Value, claims: List[VenueClaimBson],
                 lastClaim: VenueClaimBson)

   */
  @Test
  def testFindAndModify {
    val v1 = VenueR.where(_.venuename eqs "v1")
      .findAndModify(_.userid setTo 5) //all required fields have to be set, because they are required in CC
      .and(_.legacyid setTo 0L).and(_.venuename setTo "").and(_.mayor_count setTo 0L)
      .and(_.closed setTo false).and(_.last_updated setTo LocalDateTime.now())
      .and(_.status setTo VenueStatus.open).and(_.mayor setTo 0L)
      .upsertOne(returnNew = false)

    v1 must_== None
    val v2 = VenueR.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 5)
      .and(_.legacyid setTo 0L).and(_.mayor_count setTo 0L)
      .and(_.closed setTo false).and(_.last_updated setTo LocalDateTime.now())
      .and(_.status setTo VenueStatus.open).and(_.mayor setTo 0L).and(_.userid setTo 0L)
      .upsertOne(returnNew = true)

    v2.map(_.userId) must_== Some(5)

    val v3 = VenueR.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 6)
      .upsertOne(returnNew = false)
    v3.map(_.userId) must_== Some(5)

    val v4 = VenueR.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 7)
      .upsertOne(returnNew = true)
    v4.map(_.userId) must_== Some(7)

  }

  @Test
  def testRegexQuery {
    val v = baseTestVenue()
    VenueR.insertOne(v)
    VenueR.where(_.id eqs v._id).and(_.venuename startsWith "test v").count() must_== 1
    VenueR.where(_.id eqs v._id).and(_.venuename matches ".es. v".r).count() must_== 1
    VenueR.where(_.id eqs v._id).and(_.venuename matches "Tes. v".r).count() must_== 0
    VenueR.where(_.id eqs v._id).and(_.venuename matches Pattern.compile("Tes. v", Pattern.CASE_INSENSITIVE)).count() must_== 1
    VenueR.where(_.id eqs v._id).and(_.venuename matches "test .*".r).and(_.legacyid in List(v.legId)).count() must_== 1
    VenueR.where(_.id eqs v._id).and(_.venuename matches "test .*".r).and(_.legacyid nin List(v.legId)).count() must_== 0
    VenueR.where(_.tags matches """some\s.*""".r).count() must_== 1
  }

  @Test
  def testLimitAndBatch {
    (1 to 50).foreach(_ => VenueR.insertOne(baseTestVenue()))
    val q = VenueR.select(_.id)
    q.limit(10).fetch().length must_== 10
    q.limit(-10).fetch().length must_== 10
  }
  @Test
  def testCount {
    (1 to 10).foreach(_ => VenueR.insertOne(baseTestVenue()))
    val q = VenueR.select(_.id)
    q.count() must_== 10
    q.limit(3).count() must_== 3
    q.limit(15).count() must_== 10
    q.skip(5).count() must_== 5
    q.skip(12).count() must_== 0
    q.skip(3).limit(5).count() must_== 5
    q.skip(8).limit(4).count() must_== 2
  }
  // distincts need codecs for Long, and probably Int in db
  @Test
  def testDistinct {
    (1 to 5).foreach(_ => VenueR.insertOne(baseTestVenue().copy(userId = 1L)))
    (1 to 5).foreach(_ => VenueR.insertOne(baseTestVenue().copy(userId = 2L)))
    (1 to 5).foreach(_ => VenueR.insertOne(baseTestVenue().copy(userId = 3L)))
    VenueR.where(_.mayor eqs 789L).distinct(_.userid).length must_== 3
    VenueR.where(_.mayor eqs 789L).countDistinct(_.userid) must_== 3
  }

  @Test
  def testSlice(): Unit = {
    val v = baseTestVenue().copy(tags = List("1", "2", "3", "4"))
    VenueR.insertOne(v)
    VenueR.select(_.tags.slice(2)).get() must_== Some(List("1", "2"))
    VenueR.select(_.tags.slice(-2)).get() must_== Some(List("3", "4"))
    VenueR.select(_.tags.slice(1, 2)).get() must_== Some(List("2", "3"))
  }

  @Test
  def testOptFields(): Unit = {
    val v1 = OptValCC(maybes = Option("first"), realString = "Real one")
    val id = new ObjectId()
    val v2 = OptValCC(maybes = Option("second"), maybeid = Option(id), realString = "real two")
    OptValCCR.insertMany(Seq(v1, v2))
    OptValCCR.createIndex((OptValCCR.ms.name, 1))
    val strings = OptValCCR.select(_.ms).fetch()
    val ids = OptValCCR.select(_.mi).fetch()
    val rs = OptValCCR.select(_.rs).fetch()
    strings must_=== List(Some("first"), Some("second"))
    ids must_=== List(None, Some(id))
    rs must_=== List("Real one", "real two")
  }

}

