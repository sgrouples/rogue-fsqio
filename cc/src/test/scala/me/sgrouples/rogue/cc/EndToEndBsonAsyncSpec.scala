package me.sgrouples.rogue.cc
import scala.language.implicitConversions
import java.time.LocalDateTime
import java.util.{Currency, Locale}
import java.util.regex.Pattern
import org.mongodb.scala._
import me.sgrouples.rogue.cc.CcRogue.*
import me.sgrouples.rogue.cc.*
import org.bson.types.ObjectId
import munit.FunSuite
import org.mongodb.scala.result.DeleteResult

import scala.concurrent.duration._
import me.sgrouples.rogue.tags.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

class EndToEndBsonAsyncSpec extends FunSuite {
  import Metas._

  val lastClaim = VenueClaimBson(uid = 5678L, status = ClaimStatus.approved)

  def baseTestVenue(): Venue = Venue(
    _id = tag[Venue][ObjectId](new ObjectId()),
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
    VenueClaim(
      tag[VenueClaim][ObjectId](new ObjectId()),
      vid,
      123L,
      ClaimStatus.approved
    )
  }

  def baseTestTip(): Tip = {
    Tip(new ObjectId(), legid = 234L, counts = Map("foo" -> 1L, "bar" -> 2L))
  }

  private var dbOpt: Option[MongoDatabase] = None
  implicit def db:MongoDatabase = dbOpt.getOrElse(throw new RuntimeException("Uninitialized"))

  override def beforeEach(context: BeforeEach): Unit = {}

  //TODO - awaits?
  override def afterAll(): Unit = {
    dbOpt.foreach(_.drop())
    MongoTestConn.disconnectFromMongo()
    dbOpt = None
  }

  override def beforeAll(): Unit = {
    dbOpt = Some(
      MongoTestConn
        .connectToMongo()
        .getDatabase("e2e-async")
        .withCodecRegistry(CcMongo.codecRegistry)
    )
  }

  override def afterEach(context: AfterEach): Unit = {
    Await.ready(
      for {
        _ <- VenueR.bulkDeleteAsync_!!!()
        _ <- VenueClaimR.bulkDeleteAsync_!!!()
        venueRCnt <- VenueR.countAsync() //.futureValue mustBe 0L
        venueClaimCnt <- VenueClaimR.countAsync()
      } yield {
        assert(venueRCnt == 0L, s"venueR left after tests ${context.test.name}")
        assert(
          venueClaimCnt == 0L,
          "venueCLam left after test  ${context.test.name}"
        )
      },
      60.seconds
    )
  }

  test("Eqs test") {
    val v = baseTestVenue()
    for {
      _ <- VenueR.insertOneAsync(v)
      vc = baseTestVenueClaim(v._id)
      _ <- VenueClaimR.insertOneAsync(vc)
      venueRs1 <- ccMetaToQueryBuilder(VenueR)
        .where(robjectIdFieldToObjectIdQueryField(_.id) eqs v._id)
        .fetchAsync()
      _ = assertEquals(venueRs1.map(_._id), Seq(v._id))
      byMajor <- VenueR.where(_.mayor eqs v.mayor).fetchAsync()
      _ = assertEquals(byMajor.map(_._id), List(v._id))
      byVenue <- VenueR.where(_.venuename eqs v.venuename).fetchAsync()
      _ = assertEquals(byVenue.map(_._id), List(v._id))
      byClosed <- VenueR.where(_.closed eqs false).fetchAsync()
      _ = assertEquals(byClosed.map(_._id), List(v._id))
      _ <- VenueR.where(_.mayor eqs 432432).fetchAsync().map { res =>
        assertEquals(res.map(_._id), Nil)
      }
      _ <- VenueR.where(_.closed eqs true).fetchAsync().map { res =>
        assertEquals(res.map(_._id), Nil)
      }
      _ <- VenueClaimR
        .where(_.status eqs ClaimStatus.approved)
        .fetchAsync()
        .map { res => assertEquals(res.map(_._id), List(vc._id)) }
      _ <- VenueClaimR.where(_.venueid eqs v._id).fetchAsync().map { res =>
        assertEquals(res.map(_._id), List(vc._id))
      }
    } yield {
      ()
    }
  }

  test("Inequality queries") {
    val v = baseTestVenue()
    val vc = baseTestVenueClaim(v._id)
    for {
      _ <- VenueR.insertOneAsync(v)
      _ <- VenueClaimR.insertOneAsync(vc)

      // neq,lt,gt, where the lone Venue has mayor_count=3, and the only
      // VenueClaim has status approved.
      h <- VenueR.where(_.mayor_count neqs 5).fetchAsync()
      _ <- VenueR
        .where(_.mayor_count neqs 5)
        .maxTime(1.second)
        .fetchAsync()
        .map { res =>
          assertEquals(res.map(_._id), List(v._id))
        }
      _ <- VenueR.where(_.mayor_count < 5).fetchAsync().map { res =>
        assertEquals(res.map(_._id), List(v._id))
      }
      _ <- VenueR.where(_.mayor_count lt 5).fetchAsync().map { res =>
        assertEquals(res.map(_._id), List(v._id))
      }
      _ <- VenueR.where(_.mayor_count <= 5).fetchAsync().map { res =>
        assertEquals(res.map(_._id), List(v._id))
      }
      _ <- VenueR.where(_.mayor_count lte 5).fetchAsync().map { res =>
        assertEquals(res.map(_._id), List(v._id))
      }
      _ <- VenueR.where(_.mayor_count > 5).fetchAsync().map { res =>
        assertEquals(res.map(_._id), Nil)
      }
      _ <- VenueR.where(_.mayor_count gt 5).fetchAsync().map { res =>
        assertEquals(res.map(_._id), Nil)
      }
      _ <- VenueR.where(_.mayor_count >= 5).fetchAsync().map { res =>
        assertEquals(res.map(_._id), Nil)
      }
      _ <- VenueR.where(_.mayor_count gte 5).fetchAsync().map { res =>
        assertEquals(res.map(_._id), Nil)
      }
      _ <- VenueR.where(_.mayor_count between (3, 5)).fetchAsync().map { res =>
        assertEquals(res.map(_._id), List(v._id))
      }
      _ <- VenueClaimR
        .where(_.status neqs ClaimStatus.approved)
        .fetchAsync()
        .map { res => assertEquals(res.map(_._id), Nil) }
      _ <- VenueClaimR
        .where(_.status neqs ClaimStatus.pending)
        .fetchAsync()
        .map { res => assertEquals(res.map(_._id), List(vc._id)) }
    } yield {
      ()
    }
  }

  test("Select queries") {
    val v = baseTestVenue()
    val base = VenueR.where(_.id eqs v._id)

    for {
      _ <- VenueR.insertOneAsync(v)
      _ <- base.select(_.legacyid).fetchAsync().map { res =>
        assertEquals(res, List(v.legId))
      }
      _ <- base.select(_.legacyid, _.userId).fetchAsync().map { res =>
        assertEquals(res, List((v.legId, v.userId)))
      }
      _ <- base.select(_.legacyid, _.userId, _.mayor).fetchAsync().map { res =>
        assertEquals(res, List((v.legId, v.userId, v.mayor)))
      }
      _ <- base
        .select(_.legacyid, _.userId, _.mayor, _.mayor_count)
        .fetchAsync()
        .map { res =>
          assertEquals(res, List((v.legId, v.userId, v.mayor, v.mayor_count)))
        }
      _ <- base
        .select(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed)
        .fetchAsync()
        .map { res =>
          assertEquals(
            res,
            List((v.legId, v.userId, v.mayor, v.mayor_count, v.closed))
          )
        }
      _ <- base
        .select(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed, _.tags)
        .fetchAsync()
        .map { res =>
          assertEquals(
            res,
            List((v.legId, v.userId, v.mayor, v.mayor_count, v.closed, v.tags))
          )
        }
    } yield ()
  }

  test("Enum select") {
    val v = baseTestVenue()
    for {
      _ <- VenueR.insertOneAsync(v)
      res <- VenueR.where(_.id eqs v._id).select(_.status).fetchAsync()
    } yield {
      assertEquals(res, List(VenueStatus.open))
    }
  }

  test("Select case queries") {
    val v = baseTestVenue()
    val base = VenueR.where(_.id eqs v._id)
    for {
      _ <- VenueR.insertOneAsync(v)

      _ <- base.selectCase(_.legacyid, V1).fetchAsync().map {
        assertEquals(_, List(V1(v.legId)))
      }
      _ <- base.selectCase(_.legacyid, _.userId, V2).fetchAsync().map {
        assertEquals(_, List(V2(v.legId, v.userId)))
      }
      _ <- base.selectCase(_.legacyid, _.userId, _.mayor, V3).fetchAsync().map {
        assertEquals(_, List(V3(v.legId, v.userId, v.mayor)))
      }
      _ <- base
        .selectCase(_.legacyid, _.userId, _.mayor, _.mayor_count, V4)
        .fetchAsync()
        .map {
          assertEquals(_, List(V4(v.legId, v.userId, v.mayor, v.mayor_count)))
        }
      _ <- base
        .selectCase(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed, V5)
        .fetchAsync()
        .map {
          assertEquals(
            _,
            List(V5(v.legId, v.userId, v.mayor, v.mayor_count, v.closed))
          )
        }
      _ <- base
        .selectCase(
          _.legacyid,
          _.userId,
          _.mayor,
          _.mayor_count,
          _.closed,
          _.tags,
          V6
        )
        .fetchAsync()
        .map {
          assertEquals(
            _,
            List(
              V6(v.legId, v.userId, v.mayor, v.mayor_count, v.closed, v.tags)
            )
          )
        }
    } yield ()
  }

  test("Select subfield queries") {
    val v = baseTestVenue()
    val t = baseTestTip()
    val q3 = TipR.where(_.id eqs t._id).select(_.counts at "foo")

    for {
      _ <- VenueR.insertOneAsync(v)
      _ <- TipR.insertOneAsync(t)
      //TODO - no support for querying map fields now
      // select subfields
      //println(s"Q ${q.query}")
      _ <- q3.fetchAsync().map {
        assertEquals(_, Seq(Some(1L)))
      }

      //todo - no unsafe fields now
      //VenueR.where(_.id eqs v._id).select(_.geolatlng.unsafeField[Double]("lat")).fetchAsync()) mustBe List(Some(40.73))
      subuserids: Seq[Option[List[Long]]] <- VenueR
        .where(_.id eqs v._id)
        .select(_.claims.subselect(_.uid))
        .fetchAsync()
      //println(s"Sub user ids ${subuserids}")
      _ = assertEquals(subuserids, List(Some(List(1234L, 5678L))))

      q = VenueR.where(_.claims.subfield(_.uid) eqs 1234).select(_.claims.$$)
      subclaims: Seq[Seq[VenueClaimBson]] <- q.fetchAsync()
      _ = assertEquals(subclaims.size, 1)
      _ = assertEquals(subclaims.head.size, 1)
      _ = assertEquals(subclaims.head.head.uid, 1234L)
      _ = assertEquals(subclaims.head.head.status, ClaimStatus.pending)

      // selecting a claims.userid when there is no top-level claims list should
      // have one element in the List for the one Venue, but an Empty for that
      // Venue since there's no list of claims there.
      _ <- VenueR
        .where(_.id eqs v._id)
        .modify(_.claims.unset)
        .and(_.lastClaim.unset)
        .updateOneAsync()
      //val x =caseClassFieldToQueryField(VenueR.lastClaim).subfield(_.uid)

      //val d = VenueR.select(_.lastClaim.subfield(_.uid))
      //val f= roptionalFieldToSelectField(ccMetaToQueryBuilder(VenueR).select(_.lastClaim.subfield(_.uid)))
      //val q = VenueR.where(_.id eqs v._id).select(_.lastClaim.subfield(_.uid))

      _ <- VenueR
        .where(_.id eqs v._id)
        .select(_.lastClaim.subfield(_.uid))
        .fetchAsync()
        .map {
          assertEquals(_, List(None))
        }
      _ <- VenueR
        .where(_.id eqs v._id)
        .select(_.lastClaim.subselect(_.uid))
        .fetchAsync()
        .map {
          assertEquals(_, List(None))
        }
      _ <- VenueR
        .where(_.id eqs v._id)
        .select(_.claims.subselect(_.uid))
        .fetchAsync()
        .map {
          assertEquals(_, List(None))
        }
    } yield ()
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

  test("Find and modify") {

    for {
      v1 <- VenueR
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
        .upsertOneAsync(returnNew = false)
      _ = assertEquals(v1, None)
      v2 <- VenueR
        .where(_.venuename eqs "v2")
        .findAndModify(_.userId setTo 5)
        .and(_.legacyid setTo 0L)
        .and(_.mayor_count setTo 0L)
        .and(_.closed setTo false)
        .and(_.last_updated setTo LocalDateTime.now())
        .and(_.status setTo VenueStatus.open)
        .and(_.mayor setTo 0L)
        .and(_.userId setTo 0L)
        .upsertOneAsync(returnNew = true)
      _ = assertEquals(v2.map(_.userId), Some(5L))

      v3 <- VenueR
        .where(_.venuename eqs "v2")
        .findAndModify(_.userId setTo 6)
        .upsertOneAsync(returnNew = false)
      _ = assertEquals(v3.map(_.userId), Some(5L))

      v4 <- VenueR
        .where(_.venuename eqs "v2")
        .findAndModify(_.userId setTo 7)
        .upsertOneAsync(returnNew = true)
      _ = assertEquals(v4.map(_.userId), Some(7L))
    } yield ()
  }

  test("Regex query") {
    val v = baseTestVenue()
    for {
      _ <- VenueR.insertOneAsync(v)
      _ <- VenueR
        .where(_.id eqs v._id)
        .and(_.venuename startsWith "test v")
        .countAsync()
        .map { assertEquals(_, 1L) }
      _ <- VenueR
        .where(_.id eqs v._id)
        .and(_.venuename matches ".es. v".r)
        .countAsync()
        .map { assertEquals(_, 1L) }
      _ <- VenueR
        .where(_.id eqs v._id)
        .and(_.venuename matches "Tes. v".r)
        .countAsync()
        .map { assertEquals(_, 0L) }
      _ <- VenueR
        .where(_.id eqs v._id)
        .and(
          _.venuename matches Pattern
            .compile("Tes. v", Pattern.CASE_INSENSITIVE)
        )
        .countAsync()
        .map { assertEquals(_, 1L) }
      _ <- VenueR
        .where(_.id eqs v._id)
        .and(_.venuename matches "test .*".r)
        .and(_.legacyid in List(v.legId))
        .countAsync()
        .map { assertEquals(_, 1L) }
      _ <- VenueR
        .where(_.id eqs v._id)
        .and(_.venuename matches "test .*".r)
        .and(_.legacyid nin List(v.legId))
        .countAsync()
        .map { assertEquals(_, 0L) }
      _ <- VenueR.where(_.tags matches """some\s.*""".r).countAsync().map {
        assertEquals(_, 1L)
      }
    } yield ()
  }

  test("Limit and batch") {
    val q = VenueR.select(_.id)
    for {
      _ <- Future.traverse(1 to 50)(_ => VenueR.insertOneAsync(baseTestVenue()))
      _ <- q.limit(10).fetchAsync().map(res => assertEquals(res.length, 10))
      _ <- q.limit(-10).fetchAsync().map(res => assertEquals(res.length, 10))
    } yield ()
  }

  test("Count") {
    val q = VenueR.select(_.id)
    for {
      _ <- Future.sequence {
        (1 to 10).map(_ => VenueR.insertOneAsync(baseTestVenue()))
      }
      _ <- q.countAsync().map {
        assertEquals(_, 10L)
      }
      _ <- q.limit(3).countAsync().map {
        assertEquals(_, 3L)
      }
      _ <- q.limit(15).countAsync().map {
        assertEquals(_, 10L)
      }
      _ <- q.skip(5).countAsync().map {
        assertEquals(_, 5L)
      }
      _ <- q.skip(12).countAsync().map {
        assertEquals(_, 0L)
      }
      _ <- q.skip(3).limit(5).countAsync().map {
        assertEquals(_, 5L)
      }
      _ <- q.skip(8).limit(4).countAsync().map {
        assertEquals(_, 2L)
      }
    } yield ()
  }

  // distincts need codecs for Long, and probably Int in db

  test("Distinct") {
    for {
      _ <- Future.sequence(
        (1 to 5).map(_ =>
          VenueR.insertOneAsync(baseTestVenue().copy(userId = 1L))
        )
      )
      _ <- Future.sequence(
        (1 to 5).map(_ =>
          VenueR.insertOneAsync(baseTestVenue().copy(userId = 2L))
        )
      )
      _ <- Future.sequence(
        (1 to 5).map(_ =>
          VenueR.insertOneAsync(baseTestVenue().copy(userId = 3L))
        )
      )
      _ <- VenueR.where(_.mayor eqs 789L).distinctAsync(_.userId).map { res =>
        assertEquals(res.length, 3)
      }
      _ <- VenueR
        .where(_.mayor eqs 789L)
        .countDistinctAsync(_.userId)
        .map(assertEquals(_, 3L))
    } yield ()
  }
  test("Slice") {
    val v = baseTestVenue().copy(tags = List("1", "2", "3", "4"))
    for {
      _ <- VenueR.insertOneAsync(v)
      _ <- VenueR
        .select(_.tags.slice(2))
        .getAsync()
        .map(assertEquals(_, Some(List("1", "2"))))
      _ <- VenueR
        .select(_.tags.slice(-2))
        .getAsync()
        .map(assertEquals(_, Some(List("3", "4"))))
      _ <- VenueR
        .select(_.tags.slice(1, 2))
        .getAsync()
        .map(assertEquals(_, Some(List("2", "3"))))
    } yield ()
  }

  test("Select case class") {

    val v = baseTestVenue()
    val yetAnotherClaim = VenueClaimBson.default.copy(uid = 1L)
    for {
      _ <- VenueR.insertOneAsync(v)
      _ <- VenueR
        .select(_.lastClaim)
        .fetchAsync()
        .map(res => assert(res.flatten.contains(lastClaim)))
      _ <- VenueR
        .select(_.firstClaim)
        .fetchAsync()
        .map(res => assert(res.contains(VenueClaimBson.default)))
      _ <- VenueR
        .select(_.userId, _.firstClaim)
        .fetchAsync()
        .map(res => assert(res.contains(456L -> VenueClaimBson.default)))
      updatedClaim <- VenueR
        .findAndModify(_.firstClaim setTo yetAnotherClaim)
        .upsertOneAsync(returnNew = true)
      _ = assertEquals(updatedClaim.map(_.firstClaim), Some(yetAnotherClaim))
      _ <- VenueR
        .findAndModify(_.lastClaim setTo yetAnotherClaim)
        .upsertOneAsync(returnNew = true)
        .map { res =>
          assertEquals(res.flatMap(_.lastClaim), Some(yetAnotherClaim))
        }
    } yield ()
  }

  test("ReplaceOne") {
    val v1 = OptValCC(maybes = Some("bef"), realString = "ore")
    val v2 = v1.copy(maybes = None)
    for {
      _ <- OptValCCR.insertOneAsync(v1)
      _ <- OptValCCR.replaceOneAsync(v2)
      vr <- OptValCCR.where(_.id eqs v1._id).getAsync().map(_.get)
    } yield {
      assertEquals(vr.maybes, None)
      assertEquals(vr.realString, "ore")
    }
  }

  test("Currency and BigDecimal fields") {

    val USD = Currency.getInstance("USD")
    val EUR = Currency.getInstance("EUR")

    val invoice1 = Invoice(1L, "Invoice no. 1", Money(12.34, USD))
    val invoice2 = Invoice(2L, "Invoice no. 2", Money(5.12, USD))
    val invoice3 = Invoice(3L, "Invoice no. 3", Money(145.98, USD))
    val invoice4 = Invoice(4L, "Invoice no. 4", Money(99.21, EUR))

    val invoices = Seq(invoice1, invoice2, invoice3, invoice4)

    for {
      _ <- Future.traverse(invoices)(invoice =>
        Invoices.insertOneAsync(invoice)
      )
      _ <- Invoices
        .where(_.total.subfield(_.amount) eqs BigDecimal(12.34))
        .fetchAsync()
        .map(assertEquals(_, Seq(invoice1)))
      _ <- Invoices
        .where(_.id eqs 2L)
        .modify(_.total setTo Money(59.12, USD))
        .updateOneAsync()
      _ <- Invoices
        .where(_.id eqs 2L)
        .fetchAsync()
        .map(
          assertEquals(_, Seq(Invoice(2L, "Invoice no. 2", Money(59.12, USD))))
        )
      _ <- Invoices
        .where(_.total.subfield(_.currency) eqs EUR)
        .fetchAsync()
        .map(assertEquals(_, Seq(invoice4)))
      _ <- Invoices
        .where(_.id eqs 2L)
        .modify(_.total.subfield(_.currency) setTo EUR)
        .updateOneAsync()
      _ <- Invoices
        .where(_.id eqs 2L)
        .fetchAsync()
        .map(
          assertEquals(_, Seq(Invoice(2L, "Invoice no. 2", Money(59.12, EUR))))
        )
      _ <- Invoices
        .where(_.id eqs 2L)
        .modify(_.total.subfield(_.amount) setTo 1352.98)
        .updateOneAsync()
      _ <- Invoices
        .where(_.id eqs 2L)
        .fetchAsync()
        .map(
          assertEquals(
            _,
            Seq(Invoice(2L, "Invoice no. 2", Money(1352.98, EUR)))
          )
        )
    } yield ()
  }

  test("Map[K, V] field") {

    val counts = Map(ObjectId.get -> 100L)

    val counter = Counter(counts = counts)

    for {
      _ <- Counters.insertOneAsync(counter)

      countsOpt <- Counters
        .where(_.id eqs counter._id)
        .select(_.counts)
        .getAsync()
    } yield {
      val result: Map[ObjectId, Long] = countsOpt.get
      assertEquals(result, counts)
    }
  }

  test("Map[K <: ObjectId, V] field") {
    val counts: Map[CounterId, Long] = Map(tag[Counter](ObjectId.get) -> 100L)
    val counter = TypedCounter(counts = counts)

    for {
      _ <- TypedCounters.insertOneAsync(counter)

      countsOpt <- TypedCounters
        .where(_.id eqs counter._id)
        .select(_.counts)
        .getAsync()
    } yield {
      val result: Map[CounterId, Long] = countsOpt.get
      assertEquals(result, counts)
    }
  }

  test("BinaryBsonFormat") {

    val sample = BinaryData("War, war never changes".getBytes)
    for {
      _ <- Binaries.insertOneAsync(sample)
      seq: Seq[BinaryData] <- Binaries.fetchAsync()
    } yield {
      assert(
        seq.map(d => new String(d.data)).contains("War, war never changes")
      )
    }
  }

  test("LocaleBsonFormat & LocaleField") {

    val sample = LocaleData(Locale.CANADA_FRENCH)
    for {
      _ <- Locales.insertOneAsync(sample)
      seq: Seq[Locale] <- Locales
        .where(_.locale eqs Locale.CANADA_FRENCH)
        .select(_.locale)
        .fetchAsync()
      _ = assert(seq.contains(Locale.CANADA_FRENCH), "Locale not found")
      _ <- Locales
        .where(_.locale eqs Locale.CANADA_FRENCH)
        .modify(_.locale setTo Locale.CHINESE)
        .updateOneAsync()
      _ <- Locales.where(_.locale eqs Locale.CHINESE).existsAsync().map {
        assertEquals(_, true)
      }
    } yield ()
  }

  test("reactive fetch") {
    val sub = new TestSubscriber()
    VenueR
      .insertManyAsync(Seq(baseTestVenue(), baseTestVenue(), baseTestVenue()))
      .map { _ =>
        val pub =
          VenueR.where(_.closed neqs true).select(_.venuename).fetchPublisher()
        val s = pub.subscribe(sub)
        sub.waitForAll()
        val rcv = sub.getRecieved()
        assertEquals(rcv.length, 3)
        assertEquals(rcv, List("test venue", "test venue", "test venue"))
        assert(true, "OK")
      }
  }
}
