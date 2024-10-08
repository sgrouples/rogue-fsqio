package me.sgrouples.rogue.cc

import java.time.LocalDateTime
import java.util.regex.Pattern
import org.mongodb.scala._
import io.fsq.rogue._
import munit.FunSuite
import me.sgrouples.rogue.cc.CcRogue.{given, *}
import org.bson.types.ObjectId

class EndToEndBsonTest extends FunSuite {
  import Metas._

  val lastClaim = VenueClaimBson(uid = 5678L, status = ClaimStatus.approved)
  val firstClaim = VenueClaimBson(uid = 5679L, status = ClaimStatus.approved)

  def baseTestVenue(): Venue = Venue(
    _id = Venue.newId,
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
    VenueClaim(VenueClaim.newId, vid, 123L, ClaimStatus.approved)
  }

  def baseTestTip(): Tip = {
    Tip(new ObjectId(), legid = 234L, counts = Map("foo" -> 1L, "bar" -> 2L))
  }

  private var dbOpt: Option[MongoDatabase] = None
  implicit def db: MongoDatabase =
    dbOpt.getOrElse(throw new RuntimeException("UninitializedError"))

  override def beforeAll(): Unit = {
    dbOpt = Some(
      MongoTestConn
        .connectToMongo()
        .getDatabase("e2e-macro-async")
        .withCodecRegistry(CcMongo.codecRegistry)
    )
  }

  override def afterAll(): Unit = {
    dbOpt.foreach(_.drop())
    MongoTestConn.disconnectFromMongo()
    dbOpt = None
  }
  override def beforeEach(context: BeforeEach): Unit = {}

  override def afterEach(context: AfterEach): Unit = {
    VenueR.bulkDelete_!!!()
    val v = VenueR.count()
    VenueClaimR.bulkDelete_!!!()
    val vc = VenueClaimR.count()
    assert(v == 0L)
    assert(vc == 0L)
  }

  test("eqsTests") {
    val v = baseTestVenue()
    VenueR.insertOne(v)
    val vc = baseTestVenueClaim(v._id)
    VenueClaimR.insertOne(vc)

    val x = VenueR.id
    // eqs
    val q = ccMetaToQueryBuilder(VenueR).where(_.id eqs v._id)
    //println(s"Ret class ${classOf[q.meta.R]}")
    assertEquals(
      ccMetaToQueryBuilder(VenueR).where(_.id eqs v._id).fetch().map(_._id),
      Seq(v._id)
    )
    assertEquals(
      VenueR.where(_.mayor eqs v.mayor).fetch().map(_._id),
      List(v._id)
    )
    assertEquals(
      VenueR.where(_.mayor eqs v.mayor).fetch().map(_._id),
      List(v._id)
    )
    assertEquals(
      VenueR.where(_.venuename eqs v.venuename).fetch().map(_._id),
      List(v._id)
    )
    assertEquals(
      VenueR.where(_.closed eqs false).fetch().map(_._id),
      List(v._id)
    )

    assertEquals(VenueR.where(_.mayor eqs 432432).fetch().map(_._id), Nil)
    assertEquals(VenueR.where(_.closed eqs true).fetch().map(_._id), Nil)

    assertEquals(
      VenueClaimR.where(_.status eqs ClaimStatus.approved).fetch().map(_._id),
      List(vc._id)
    )
    assertEquals(
      VenueClaimR.where(_.venueid eqs v._id).fetch().map(_._id),
      List(vc._id)
    )
  }

  test("InequalityQueries") {
    val v = baseTestVenue()
    VenueR.insertOne(v)
    val vc = baseTestVenueClaim(v._id)
    VenueClaimR.insertOne(vc)

    // neq,lt,gt, where the lone Venue has mayor_count=3, and the only
    // VenueClaim has status approved.
    val h = VenueR.where(_.mayor_count neqs 5).fetch()
    assertEquals(
      VenueR.where(_.mayor_count neqs 5).fetch().map(_._id),
      List(v._id)
    )
    assertEquals(
      VenueR.where(_.mayor_count < 5).fetch().map(_._id),
      List(v._id)
    )
    assertEquals(
      VenueR.where(_.mayor_count lt 5).fetch().map(_._id),
      List(v._id)
    )
    assertEquals(
      VenueR.where(_.mayor_count <= 5).fetch().map(_._id),
      List(v._id)
    )
    assertEquals(
      VenueR.where(_.mayor_count lte 5).fetch().map(_._id),
      List(v._id)
    )
    assertEquals(VenueR.where(_.mayor_count > 5).fetch().map(_._id), Nil)
    assertEquals(VenueR.where(_.mayor_count gt 5).fetch().map(_._id), Nil)
    assertEquals(VenueR.where(_.mayor_count >= 5).fetch().map(_._id), Nil)
    assertEquals(VenueR.where(_.mayor_count gte 5).fetch().map(_._id), Nil)
    assertEquals(
      VenueR.where(_.mayor_count between (3, 5)).fetch().map(_._id),
      List(v._id)
    )
    assertEquals(
      VenueClaimR.where(_.status neqs ClaimStatus.approved).fetch().map(_._id),
      Nil
    )
    assertEquals(
      VenueClaimR.where(_.status neqs ClaimStatus.pending).fetch().map(_._id),
      List(vc._id)
    )
  }

  test("selectQueries") {
    val v = baseTestVenue()
    VenueR.insertOne(v)

    val base = VenueR.where(_.id eqs v._id)
    //val f = mandatoryFieldToSelectField(VenueR.legacyid)

    assertEquals(base.select(_.legacyid).fetch(), List(v.legId))

    assertEquals(
      base.select(_.legacyid, _.userId).fetch(),
      List((v.legId, v.userId))
    )
    assertEquals(
      base.select(_.legacyid, _.userId, _.mayor).fetch(),
      List((v.legId, v.userId, v.mayor))
    )
    assertEquals(
      base.select(_.legacyid, _.userId, _.mayor, _.mayor_count).fetch(),
      List((v.legId, v.userId, v.mayor, v.mayor_count))
    )
    assertEquals(
      base
        .select(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed)
        .fetch(),
      List((v.legId, v.userId, v.mayor, v.mayor_count, v.closed))
    )
    assertEquals(
      base
        .select(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed, _.tags)
        .fetch(),
      List((v.legId, v.userId, v.mayor, v.mayor_count, v.closed, v.tags))
    )
  }

  test("selectEnum") {
    val v = baseTestVenue()
    VenueR.insertOne(v)
    assertEquals(
      VenueR.where(_.id eqs v._id).select(_.status).fetch(),
      List(VenueStatus.open)
    )
  }

  test("selectCaseQueries") {
    val v = baseTestVenue()
    VenueR.insertOne(v)

    val base = VenueR.where(_.id eqs v._id)
    assertEquals(
      base.selectCase(_.legacyid, V1.apply).fetch(),
      List(V1(v.legId))
    )
    assertEquals(
      base.selectCase(_.legacyid, _.userId, V2.apply).fetch(),
      List(V2(v.legId, v.userId))
    )
    assertEquals(
      base.selectCase(_.legacyid, _.userId, _.mayor, V3.apply).fetch(),
      List(V3(v.legId, v.userId, v.mayor))
    )
    assertEquals(
      base
        .selectCase(_.legacyid, _.userId, _.mayor, _.mayor_count, V4.apply)
        .fetch(),
      List(V4(v.legId, v.userId, v.mayor, v.mayor_count))
    )
    assertEquals(
      base
        .selectCase(
          _.legacyid,
          _.userId,
          _.mayor,
          _.mayor_count,
          _.closed,
          V5.apply
        )
        .fetch(),
      List(V5(v.legId, v.userId, v.mayor, v.mayor_count, v.closed))
    )
    assertEquals(
      base
        .selectCase(
          _.legacyid,
          _.userId,
          _.mayor,
          _.mayor_count,
          _.closed,
          _.tags,
          V6.apply
        )
        .fetch(),
      List(V6(v.legId, v.userId, v.mayor, v.mayor_count, v.closed, v.tags))
    )
  }

  test("selectSubfieldQueries") {
    val v = baseTestVenue()
    VenueR.insertOne(v)
    val t = baseTestTip()
    TipR.insertOne(t)

    //TODO - no support for querying map fields now
    // select subfields
    val q3 = TipR.where(_.id eqs t._id).select(_.counts at "foo")
    //println(s"Q ${q.query}")
    assertEquals(q3.fetch(), Seq(Some(1L)))

    //todo - no unsafe fields now
    //assertEquals(    VenueR.where(_.id eqs v._id).select(_.geolatlng.unsafeField[Double]("lat").fetch(), List(Some(40.73))
    val subuserids: Seq[Option[List[Long]]] =
      VenueR.where(_.id eqs v._id).select(_.claims.subselect(_.uid)).fetch()
    //println(s"Sub user ids ${subuserids}")
    assertEquals(subuserids, List(Some(List(1234L, 5678L))))

    val q = VenueR.where(_.claims.subfield(_.uid) eqs 1234).select(_.claims.$$)
    val subclaims: Seq[Seq[VenueClaimBson]] = q.fetch()
    assertEquals(subclaims.size, 1)
    assertEquals(subclaims.head.size, 1)
    assertEquals(subclaims.head.head.uid, 1234L)
    assertEquals(subclaims.head.head.status, ClaimStatus.pending)

    // selecting a claims.userid when there is no top-level claims list should
    // have one element in the List for the one Venue, but an Empty for that
    // Venue since there's no list of claims there.
    VenueR
      .where(_.id eqs v._id)
      .modify(_.claims.unset)
      .and(_.lastClaim.unset)
      .updateOne()
    //val x =caseClassFieldToQueryField(VenueR.lastClaim).subfield(_.uid)

    //val d = VenueR.select(_.lastClaim.subfield(_.uid)
    //val f= roptionalFieldToSelectField(ccMetaToQueryBuilder(VenueR).select(_.lastClaim.subfield(_.uid))
    //val q = VenueR.where(_.id eqs v._id).select(_.lastClaim.subfield(_.uid)

    assertEquals(
      VenueR.where(_.id eqs v._id).select(_.lastClaim.subfield(_.uid)).fetch(),
      List(None)
    )
    assertEquals(
      VenueR.where(_.id eqs v._id).select(_.lastClaim.subselect(_.uid)).fetch(),
      List(None)
    )
    assertEquals(
      VenueR.where(_.id eqs v._id).select(_.claims.subselect(_.uid)).fetch(),
      List(None)
    )
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
assertEquals(      statuses, List(Some("Approved"))
      // This assertion is what we want, and it fails.
assertEquals(      // statuses, List(Some(ClaimStatus.approved))

      val subuseridsAndStatuses: Seq[(Option[List[Long]], Option[List[VenueClaimBson.status.MyType]])] =
        VenueR.where(_._id eqs v._id)
          .select(_.claims.subselect(_.userid), _.claims.subselect(_.status)
          .fetch()
      // This assertion works.
assertEquals(      subuseridsAndStatuses, Seq((Some(List(1234, 5678), Some(List("Pending approval", "Approved")))

      // This assertion is what we want, and it fails.
assertEquals(      // subuseridsAndStatuses, List((Some(List(1234, 5678), Some(List(ClaimStatus.pending, ClaimStatus.approved)))
    }

    @Test
    def testReadPreference: Unit = {
      // Note: this isn't a real test of readpreference because the test mongo setup
      // doesn't have replicas. This basically just makes sure that readpreference
      // doesn't break everything.
          val v = baseTestVenue()
          VenueR.insertOne(v)

      // eqs
assertEquals(      VenueR.where(_._id eqs v._id).fetch().map(_._id), Seq(v._id))
assertEquals(      VenueR.where(_._id eqs v._id).setReadPreference(ReadPreference.secondary).fetch().map(_._id), Seq(v._id))
assertEquals(      VenueR.where(_._id eqs v._id).setReadPreference(ReadPreference.primary).fetch().map(_._id), Seq(v._id))
    }
   */

  /* Orignal test was broken - Veny has lots more required parameters than just userId
  case class Venue(_id: ObjectId, legId: Long, userId: Long, venuename: String, mayor: Long, mayor_count: Long, closed: Boolean, tags: List[String],
                 popularity: List[Long], categories: List[ObjectId], latlng: LatLong, last_updated: LocalDateTime, status: VenueStatus.Value, claims: List[VenueClaimBson],
                 lastClaim: VenueClaimBson)

   */

  test("FindAndModify") {
    val v1 = VenueR
      .where(_.venuename eqs "v1")
      .findAndModify(
        _.userId setTo 5
      ) //all required fields have to be set, because they are required in CC
      .and(_.legacyid setTo 0L)
      .and(_.venuename setTo "")
      .and(_.mayor_count setTo 0L)
      .and(_.closed setTo false)
      .and(_.last_updated setTo LocalDateTime.now())
      .and(_.status setTo VenueStatus.open)
      .and(_.mayor setTo 0L)
      .upsertOne(returnNew = false)

    assertEquals(v1, None)
    val v2 = VenueR
      .where(_.venuename eqs "v2")
      .findAndModify(_.userId setTo 5)
      .and(_.legacyid setTo 0L)
      .and(_.mayor_count setTo 0L)
      .and(_.closed setTo false)
      .and(_.last_updated setTo LocalDateTime.now())
      .and(_.status setTo VenueStatus.open)
      .and(_.mayor setTo 0L)
      .and(_.userId setTo 0L)
      .upsertOne(returnNew = true)

    assertEquals(v2.map(_.userId), Some(5L))

    val v3 = VenueR
      .where(_.venuename eqs "v2")
      .findAndModify(_.userId setTo 6)
      .upsertOne(returnNew = false)
    assertEquals(v3.map(_.userId), Some(5L))

    val v4 = VenueR
      .where(_.venuename eqs "v2")
      .findAndModify(_.userId setTo 7)
      .upsertOne(returnNew = true)
    assertEquals(v4.map(_.userId), Some(7L))

  }

  test("RegexQuery") {
    val v = baseTestVenue()
    VenueR.insertOne(v)
    assertEquals(
      VenueR.where(_.id eqs v._id).and(_.venuename startsWith "test v").count(),
      1L
    )
    assertEquals(
      VenueR.where(_.id eqs v._id).and(_.venuename matches ".es. v".r).count(),
      1L
    )
    assertEquals(
      VenueR.where(_.id eqs v._id).and(_.venuename matches "Tes. v".r).count(),
      0L
    )
    assertEquals(
      VenueR
        .where(_.id eqs v._id)
        .and(
          _.venuename matches Pattern
            .compile("Tes. v", Pattern.CASE_INSENSITIVE)
        )
        .count(),
      1L
    )
    assertEquals(
      VenueR
        .where(_.id eqs v._id)
        .and(_.venuename matches "test .*".r)
        .and(_.legacyid in List(v.legId))
        .count(),
      1L
    )
    assertEquals(
      VenueR
        .where(_.id eqs v._id)
        .and(_.venuename matches "test .*".r)
        .and(_.legacyid nin List(v.legId))
        .count(),
      0L
    )
    assertEquals(VenueR.where(_.tags matches """some\s.*""".r).count(), 1L)
  }

  test("LimitAndBatch") {
    (1 to 50).foreach(_ => VenueR.insertOne(baseTestVenue()))
    val q = VenueR.select(_.id)
    assertEquals(q.limit(10).fetch().length, 10)
    assertEquals(q.limit(-10).fetch().length, 10)
  }

  test("Count") {
    (1 to 10).foreach(_ => VenueR.insertOne(baseTestVenue()))
    val q = VenueR.select(_.id)
    assertEquals(q.count(), 10L)
    assertEquals(q.limit(3).count(), 3L)
    assertEquals(q.limit(15).count(), 10L)
    assertEquals(q.skip(5).count(), 5L)
    assertEquals(q.skip(12).count(), 0L)
    assertEquals(q.skip(3).limit(5).count(), 5L)
    assertEquals(q.skip(8).limit(4).count(), 2L)
  }
  // distincts need codecs for Long, and probably Int in db

  test("Distinct") {
    (1 to 5).foreach(_ => VenueR.insertOne(baseTestVenue().copy(userId = 1L)))
    (1 to 5).foreach(_ => VenueR.insertOne(baseTestVenue().copy(userId = 2L)))
    (1 to 5).foreach(_ => VenueR.insertOne(baseTestVenue().copy(userId = 3L)))
    assertEquals(VenueR.where(_.mayor eqs 789L).distinct(_.userId).length, 3)
    assertEquals(VenueR.where(_.mayor eqs 789L).countDistinct(_.userId), 3L)
  }

  test("Slice") {
    val v = baseTestVenue().copy(tags = List("1", "2", "3", "4"))
    VenueR.insertOne(v)
    assertEquals(VenueR.select(_.tags.slice(2)).get(), Some(List("1", "2")))
    assertEquals(VenueR.select(_.tags.slice(-2)).get(), Some(List("3", "4")))
    assertEquals(VenueR.select(_.tags.slice(1, 2)).get(), Some(List("2", "3")))
  }

  test("OptFields") {
    val v1 = OptValCC(maybes = Option("first"), realString = "Real one")
    val id = new ObjectId()
    val v2 = OptValCC(
      maybes = Option("second"),
      maybeid = Option(id),
      realString = "real two"
    )
    OptValCCR.insertMany(Seq(v1, v2))
    OptValCCR.createIndex((OptValCCR.ms.name, 1))
    val strings = OptValCCR.select(_.ms).fetch()
    val ids = OptValCCR.select(_.mi).fetch()
    val rs = OptValCCR.select(_.rs).fetch()
    assertEquals(strings, List(Some("first"), Some("second")))
    assertEquals(ids, List(None, Some(id)))
    assertEquals(rs, List("Real one", "real two"))
  }

  test("replaceOneTest") {
    val v1 = OptValCC(maybes = Some("bef"), realString = "ore")
    val v2 = v1.copy(maybes = None)
    OptValCCR.insertOne(v1)
    OptValCCR.replaceOne(v2)
    val vr = OptValCCR.where(_.id eqs v1._id).get().get
    assertEquals(vr.maybes, None)
    assertEquals(vr.realString, "ore")
  }

}
