package me.sgrouples.rogue.cc

import java.time.LocalDateTime
import java.util.regex.Pattern

import com.mongodb.async.client.MongoDatabase
import me.sgrouples.rogue.cc.CcRogue._
import org.bson.types.ObjectId
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ BeforeAndAfterEach, FlatSpec, MustMatchers }

import scala.concurrent.duration._

class EndToEndBsonAsyncSpec extends FlatSpec with MustMatchers with ScalaFutures with BeforeAndAfterEach {
  import Metas._

  implicit val atMost = PatienceConfig(15 seconds)

  val lastClaim = VenueClaimBson(uid = 5678L, status = ClaimStatus.approved)

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

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    val m = MongoTestConn.connectToMongo
    CcMongo.defineDb("lift", m, "rogue-test-async")
    dbOpt = CcMongo.getDb("lift")
  }

  override protected def afterEach(): Unit = {
    super.afterEach()

    VenueR.bulkDeleteAsync_!!!().futureValue
    VenueR.countAsync().futureValue mustBe 0L

    VenueClaimR.bulkDeleteAsync_!!!().futureValue
    VenueClaimR.countAsync().futureValue mustBe 0L

    //Like.allShards.bulkDeleteAsync_!!!())

    MongoTestConn.disconnectFromMongo
  }

  "Eqs test" should "work as expected" in {
    val v = baseTestVenue()
    VenueR.insertOneAsync(v).futureValue
    val vc = baseTestVenueClaim(v._id)
    VenueClaimR.insertOneAsync(vc).futureValue

    ccMetaToQueryBuilder(VenueR).where(_.id eqs v._id).fetchAsync().futureValue.map(_._id) mustBe Seq(v._id)
    VenueR.where(_.mayor eqs v.mayor).fetchAsync().futureValue.map(_._id) mustBe List(v._id)
    VenueR.where(_.mayor eqs v.mayor).fetchAsync().futureValue.map(_._id) mustBe List(v._id)
    VenueR.where(_.venuename eqs v.venuename).fetchAsync().futureValue.map(_._id) mustBe List(v._id)
    VenueR.where(_.closed eqs false).fetchAsync().futureValue.map(_._id) mustBe List(v._id)

    VenueR.where(_.mayor eqs 432432).fetchAsync().futureValue.map(_._id) mustBe Nil
    VenueR.where(_.closed eqs true).fetchAsync().futureValue.map(_._id) mustBe Nil

    VenueClaimR.where(_.status eqs ClaimStatus.approved).fetchAsync().futureValue.map(_._id) mustBe List(vc._id)
    VenueClaimR.where(_.venueid eqs v._id).fetchAsync().futureValue.map(_._id) mustBe List(vc._id)
  }

  "Inequality queries" should "work as expected" in {
    val v = baseTestVenue()
    VenueR.insertOneAsync(v).futureValue
    val vc = baseTestVenueClaim(v._id)
    VenueClaimR.insertOneAsync(vc).futureValue

    // neq,lt,gt, where the lone Venue has mayor_count=3, and the only
    // VenueClaim has status approved.
    val h = VenueR.where(_.mayor_count neqs 5).fetchAsync()
    VenueR.where(_.mayor_count neqs 5).fetchAsync().futureValue.map(_._id) mustBe List(v._id)
    VenueR.where(_.mayor_count < 5).fetchAsync().futureValue.map(_._id) mustBe List(v._id)
    VenueR.where(_.mayor_count lt 5).fetchAsync().futureValue.map(_._id) mustBe List(v._id)
    VenueR.where(_.mayor_count <= 5).fetchAsync().futureValue.map(_._id) mustBe List(v._id)
    VenueR.where(_.mayor_count lte 5).fetchAsync().futureValue.map(_._id) mustBe List(v._id)
    VenueR.where(_.mayor_count > 5).fetchAsync().futureValue.map(_._id) mustBe Nil
    VenueR.where(_.mayor_count gt 5).fetchAsync().futureValue.map(_._id) mustBe Nil
    VenueR.where(_.mayor_count >= 5).fetchAsync().futureValue.map(_._id) mustBe Nil
    VenueR.where(_.mayor_count gte 5).fetchAsync().futureValue.map(_._id) mustBe Nil
    VenueR.where(_.mayor_count between (3, 5)).fetchAsync().futureValue.map(_._id) mustBe List(v._id)
    VenueClaimR.where(_.status neqs ClaimStatus.approved).fetchAsync().futureValue.map(_._id) mustBe Nil
    VenueClaimR.where(_.status neqs ClaimStatus.pending).fetchAsync().futureValue.map(_._id) mustBe List(vc._id)
  }

  "Select queries" should "work as expected" in {
    val v = baseTestVenue()
    VenueR.insertOneAsync(v).futureValue

    val base = VenueR.where(_.id eqs v._id)
    //val f = mandatoryFieldToSelectField(VenueR.legacyid)

    base.select(_.legacyid).fetchAsync().futureValue mustBe List(v.legId)

    base.select(_.legacyid, _.userid).fetchAsync().futureValue mustBe List((v.legId, v.userId))
    base.select(_.legacyid, _.userid, _.mayor).fetchAsync().futureValue mustBe List((v.legId, v.userId, v.mayor))
    base.select(_.legacyid, _.userid, _.mayor, _.mayor_count).fetchAsync().futureValue mustBe List((v.legId, v.userId, v.mayor, v.mayor_count))
    base.select(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed).fetchAsync().futureValue mustBe List((v.legId, v.userId, v.mayor, v.mayor_count, v.closed))
    base.select(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, _.tags).fetchAsync().futureValue mustBe List((v.legId, v.userId, v.mayor, v.mayor_count, v.closed, v.tags))

  }

  "Enum select" should "work as expected" in {
    val v = baseTestVenue()
    VenueR.insertOneAsync(v).futureValue
    VenueR.where(_.id eqs v._id).select(_.status).fetchAsync().futureValue mustBe List(VenueStatus.open)
  }

  "Select case queries" should "work as expected" in {
    val v = baseTestVenue()
    VenueR.insertOneAsync(v).futureValue

    val base = VenueR.where(_.id eqs v._id)
    base.selectCase(_.legacyid, V1).fetchAsync().futureValue mustBe List(V1(v.legId))
    base.selectCase(_.legacyid, _.userid, V2).fetchAsync().futureValue mustBe List(V2(v.legId, v.userId))
    base.selectCase(_.legacyid, _.userid, _.mayor, V3).fetchAsync().futureValue mustBe List(V3(v.legId, v.userId, v.mayor))
    base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, V4).fetchAsync().futureValue mustBe List(V4(v.legId, v.userId, v.mayor, v.mayor_count))
    base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, V5).fetchAsync().futureValue mustBe List(V5(v.legId, v.userId, v.mayor, v.mayor_count, v.closed))
    base.selectCase(_.legacyid, _.userid, _.mayor, _.mayor_count, _.closed, _.tags, V6).fetchAsync().futureValue mustBe List(V6(v.legId, v.userId, v.mayor, v.mayor_count, v.closed, v.tags))

  }

  "Select subfield queries" should "work as expected" in {
    val v = baseTestVenue()
    VenueR.insertOneAsync(v).futureValue
    val t = baseTestTip()
    TipR.insertOneAsync(t).futureValue

    //TODO - no support for querying map fields now
    // select subfields
    val q3 = TipR.where(_.id eqs t._id).select(_.counts at "foo")
    //println(s"Q ${q.query}")
    q3.fetchAsync().futureValue mustBe Seq(Some(1L))

    //todo - no unsafe fields now
    //VenueR.where(_.id eqs v._id).select(_.geolatlng.unsafeField[Double]("lat")).fetchAsync()) mustBe List(Some(40.73))
    val subuserids: Seq[Option[List[Long]]] = VenueR.where(_.id eqs v._id)
      .select(_.claims.subselect(_.uid))
      .fetchAsync()
      .futureValue
    //println(s"Sub user ids ${subuserids}")
    subuserids mustBe List(Some(List(1234, 5678)))

    val q = VenueR.where(_.claims.subfield(_.uid) eqs 1234).select(_.claims.$$)
    val subclaims: Seq[Seq[VenueClaimBson]] = q.fetchAsync().futureValue
    subclaims.size mustBe 1
    subclaims.head.size mustBe 1
    subclaims.head.head.uid mustBe 1234
    subclaims.head.head.status mustBe ClaimStatus.pending

    // selecting a claims.userid when there is no top-level claims list should
    // have one element in the List for the one Venue, but an Empty for that
    // Venue since there's no list of claims there.
    VenueR.where(_.id eqs v._id).modify(_.claims unset).and(_.lastClaim unset).updateOneAsync().futureValue
    //val x =caseClassFieldToQueryField(VenueR.lastClaim).subfield(_.uid)

    //val d = VenueR.select(_.lastClaim.subfield(_.uid))
    //val f= roptionalFieldToSelectField(ccMetaToQueryBuilder(VenueR).select(_.lastClaim.subfield(_.uid)))
    //val q = VenueR.where(_.id eqs v._id).select(_.lastClaim.subfield(_.uid))

    VenueR.where(_.id eqs v._id).select(_.lastClaim.subfield(_.uid)).fetchAsync().futureValue mustBe List(None)
    VenueR.where(_.id eqs v._id).select(_.lastClaim.subselect(_.uid)).fetchAsync().futureValue mustBe List(None)
    VenueR.where(_.id eqs v._id).select(_.claims.subselect(_.uid)).fetchAsync().futureValue mustBe List(None)
  }
  /*
  @Ignore("These tests are broken because DummyField doesn't know how to convert a String to an Enum")
  def testSelectEnumSubfield: Unit = {
        val v = baseTestVenue()
        VenueR.insertOneAsync(v))

    // This behavior is broken because we get a String back from mongo, and at
    // that point we only have a DummyField for the subfield, and that doesn't
    // know how to convert the String to an Enum.

    val statuses: Seq[Option[VenueClaimBson.status.MyType]] =
      VenueR.where(_._id eqs v._id).select(_.lastClaim.subselect(_.status)).fetchAsync())
    // This assertion works.
    statuses mustBe List(Some("Approved"))
    // This assertion is what we want, and it fails.
    // statuses mustBe List(Some(ClaimStatus.approved))

    val subuseridsAndStatuses: Seq[(Option[List[Long]], Option[List[VenueClaimBson.status.MyType]])] =
      VenueR.where(_._id eqs v._id)
        .select(_.claims.subselect(_.userid), _.claims.subselect(_.status))
        .fetchAsync())
    // This assertion works.
    subuseridsAndStatuses mustBe Seq((Some(List(1234, 5678)), Some(List("Pending approval", "Approved"))))

    // This assertion is what we want, and it fails.
    // subuseridsAndStatuses mustBe List((Some(List(1234, 5678)), Some(List(ClaimStatus.pending, ClaimStatus.approved))))
  }

  @Test
  def testReadPreference: Unit = {
    // Note: this isn't a real test of readpreference because the test mongo setup
    // doesn't have replicas. This basically just makes sure that readpreference
    // doesn't break everything.
        val v = baseTestVenue()
        VenueR.insertOneAsync(v))

    // eqs
    VenueR.where(_._id eqs v._id).fetchAsync()).map(_._id) mustBe Seq(v._id)
    VenueR.where(_._id eqs v._id).setReadPreference(ReadPreference.secondary).fetchAsync()).map(_._id) mustBe Seq(v._id)
    VenueR.where(_._id eqs v._id).setReadPreference(ReadPreference.primary).fetchAsync()).map(_._id) mustBe Seq(v._id)
  }
*/

  /* Orignal test was broken - Veny has lots more required parameters than just userId
  case class Venue(_id: ObjectId, legId: Long, userId: Long, venuename: String, mayor: Long, mayor_count: Long, closed: Boolean, tags: List[String],
                 popularity: List[Long], categories: List[ObjectId], latlng: LatLong, last_updated: LocalDateTime, status: VenueStatus.Value, claims: List[VenueClaimBson],
                 lastClaim: VenueClaimBson)

   */

  "Find and modify" should "work as expected" in {

    val v1 = VenueR.where(_.venuename eqs "v1")
      .findAndModify(_.userid setTo 5) //all required fields have to be set, because they are required in CC
      .and(_.legacyid setTo 0L).and(_.venuename setTo "").and(_.mayor_count setTo 0L)
      .and(_.closed setTo false).and(_.last_updated setTo LocalDateTime.now())
      .and(_.status setTo VenueStatus.open).and(_.mayor setTo 0L)
      .upsertOneAsync(returnNew = false).futureValue

    v1 mustBe None
    val v2 = VenueR.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 5)
      .and(_.legacyid setTo 0L).and(_.mayor_count setTo 0L)
      .and(_.closed setTo false).and(_.last_updated setTo LocalDateTime.now())
      .and(_.status setTo VenueStatus.open).and(_.mayor setTo 0L).and(_.userid setTo 0L)
      .upsertOneAsync(returnNew = true).futureValue

    v2.map(_.userId) mustBe Some(5)

    val v3 = VenueR.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 6)
      .upsertOneAsync(returnNew = false).futureValue
    v3.map(_.userId) mustBe Some(5)

    val v4 = VenueR.where(_.venuename eqs "v2")
      .findAndModify(_.userid setTo 7)
      .upsertOneAsync(returnNew = true).futureValue
    v4.map(_.userId) mustBe Some(7)
  }

  "Regex query" should "work as expected" in {
    val v = baseTestVenue()
    VenueR.insertOneAsync(v).futureValue
    VenueR.where(_.id eqs v._id).and(_.venuename startsWith "test v").countAsync().futureValue mustBe 1
    VenueR.where(_.id eqs v._id).and(_.venuename matches ".es. v".r).countAsync().futureValue mustBe 1
    VenueR.where(_.id eqs v._id).and(_.venuename matches "Tes. v".r).countAsync().futureValue mustBe 0
    VenueR.where(_.id eqs v._id).and(_.venuename matches Pattern.compile("Tes. v", Pattern.CASE_INSENSITIVE)).countAsync().futureValue mustBe 1
    VenueR.where(_.id eqs v._id).and(_.venuename matches "test .*".r).and(_.legacyid in List(v.legId)).countAsync().futureValue mustBe 1
    VenueR.where(_.id eqs v._id).and(_.venuename matches "test .*".r).and(_.legacyid nin List(v.legId)).countAsync().futureValue mustBe 0
    VenueR.where(_.tags matches """some\s.*""".r).countAsync().futureValue mustBe 1

  }

  "Limit and batch" should "work as expected" in {
    (1 to 50).foreach(_ => VenueR.insertOneAsync(baseTestVenue()).futureValue)
    val q = VenueR.select(_.id)
    q.limit(10).fetchAsync().futureValue.length mustBe 10
    q.limit(-10).fetchAsync().futureValue.length mustBe 10
  }

  "Count" should "work as expected" in {
    (1 to 10).foreach(_ => VenueR.insertOneAsync(baseTestVenue()).futureValue)
    val q = VenueR.select(_.id)
    q.countAsync().futureValue mustBe 10
    q.limit(3).countAsync().futureValue mustBe 3
    q.limit(15).countAsync().futureValue mustBe 10
    q.skip(5).countAsync().futureValue mustBe 5
    q.skip(12).countAsync().futureValue mustBe 0
    q.skip(3).limit(5).countAsync().futureValue mustBe 5
    q.skip(8).limit(4).countAsync().futureValue mustBe 2
  }

  // distincts need codecs for Long, and probably Int in db

  "Distinct" should "work as expected" in {
    (1 to 5).foreach(_ => VenueR.insertOneAsync(baseTestVenue().copy(userId = 1L)).futureValue)
    (1 to 5).foreach(_ => VenueR.insertOneAsync(baseTestVenue().copy(userId = 2L)).futureValue)
    (1 to 5).foreach(_ => VenueR.insertOneAsync(baseTestVenue().copy(userId = 3L)).futureValue)
    VenueR.where(_.mayor eqs 789L).distinctAsync(_.userid).futureValue.length mustBe 3
    VenueR.where(_.mayor eqs 789L).countDistinctAsync(_.userid).futureValue mustBe 3
  }

  "Slice" should "work as expected" in {
    val v = baseTestVenue().copy(tags = List("1", "2", "3", "4"))
    VenueR.insertOneAsync(v).futureValue
    VenueR.select(_.tags.slice(2)).getAsync().futureValue mustBe Some(List("1", "2"))
    VenueR.select(_.tags.slice(-2)).getAsync().futureValue mustBe Some(List("3", "4"))
    VenueR.select(_.tags.slice(1, 2)).getAsync().futureValue mustBe Some(List("2", "3"))
  }

  "Select cc" should "work as expected" in {

    val v = baseTestVenue()
    VenueR.insertOneAsync(v).futureValue

    VenueR.select(_.lastClaim).fetchAsync()
      .futureValue.flatten must contain(lastClaim)

    VenueR.select(_.firstClaim).fetchAsync()
      .futureValue must contain(VenueClaimBson.default)

    VenueR.select(_.userid, _.firstClaim).fetchAsync()
      .futureValue must contain(456L -> VenueClaimBson.default)

    val yetAnotherClaim = VenueClaimBson.default.copy(uid = 1L)

    VenueR.findAndModify(_.firstClaim setTo yetAnotherClaim)
      .upsertOneAsync(returnNew = true)
      .futureValue.map(_.firstClaim) mustBe Some(yetAnotherClaim)

    VenueR.findAndModify(_.lastClaim setTo yetAnotherClaim)
      .upsertOneAsync(returnNew = true)
      .futureValue.flatMap(_.lastClaim) mustBe Some(yetAnotherClaim)

  }

  "ReplaceOne" should "replace value" in {
    val v1 = OptValCC(maybes = Some("bef"), realString = "ore")
    val v2 = v1.copy(maybes = None)
    OptValCCR.insertOneAsync(v1).futureValue
    OptValCCR.replaceOneAsync(v2).futureValue
    val vr = OptValCCR.where(_.id eqs v1._id).getAsync().futureValue.get
    vr.maybes mustBe None
    vr.realString must ===("ore")
  }

}

