package me.sgrouples.rogue.cc

// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.

import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.util.UUID
import io.fsq.rogue._
import CcRogue._
import munit.FunSuite

import java.util.regex.Pattern

import io.fsq.field.Field
import me.sgrouples.rogue.CClassListField
import me.sgrouples.rogue.cc.Metas._
import org.bson.types._
import shapeless.tag
import shapeless.tag.@@
import me.sgrouples.rogue.QueryParser._

class QueryTest extends FunSuite {

  def oidFromLocalDateTime(d: LocalDateTime): ObjectId = {
    CcMongo.oidFromInstant(d.toInstant(ZoneOffset.UTC))
  }

  test("ProduceACorrectJSONQueryString") {
    val d1 = LocalDateTime.of(2010, 5, 1, 0, 0, 0, 0)
    val d2 = LocalDateTime.of(2010, 5, 2, 0, 0, 0, 0)
    val oid1 = oidFromLocalDateTime(d1)
    val oid2 = oidFromLocalDateTime(d2)
    val oid = new ObjectId
    case class Ven1(id: ObjectId @@ Venue)
    val ven1 = Ven1(tag[Venue](oid1))

    // eqs
    assertEquals(
      VenueR.where(_.mayor eqs 1).q,
      pq("""db.venues.find({"mayor": {"$numberLong": "1"}})""")
    )
    assertEquals(
      VenueR.where(_.venuename eqs "Starbucks").q,
      pq("""db.venues.find({"venuename": "Starbucks"})""")
    )
    assertEquals(
      VenueR.where(_.closed eqs true).q,
      pq("""db.venues.find({"closed": true})""")
    )
    assertEquals(
      VenueR.where(_.id eqs tag[Venue](oid)).q,
      pq(("""db.venues.find({"_id": {"$oid": "%s"}})""" format oid.toString))
    )
    assertEquals(
      VenueClaimR.where(_.status eqs ClaimStatus.approved).q,
      pq("""db.venueclaims.find({"status": "Approved"})""")
    )

    assertEquals(
      VenueClaimR.where(_.venueid eqs tag[Venue](oid)).q,
      pq(
        ("""db.venueclaims.find({"vid": {"$oid": "%s"}})""" format oid.toString)
      )
    )
    assertEquals(
      VenueClaimR.where(_.venueid eqs tag[Venue](ven1.id)).q,
      pq(
        ("""db.venueclaims.find({"vid": {"$oid": "%s"}})""" format oid1.toString)
      )
    )

    // neq,lt,gt
    assertEquals(
      VenueR.where(_.mayor_count neqs 5).q,
      pq("""db.venues.find({"mayor_count": {"$ne": {"$numberLong": "5"}}})""")
    )
    assertEquals(
      VenueR.where(_.mayor_count < 5).q,
      pq("""db.venues.find({"mayor_count": {"$lt": {"$numberLong": "5"}}})""")
    )
    assertEquals(
      VenueR.where(_.mayor_count lt 5).q,
      pq("""db.venues.find({"mayor_count": {"$lt": {"$numberLong": "5"}}})""")
    )
    assertEquals(
      VenueR.where(_.mayor_count <= 5).q,
      pq("""db.venues.find({"mayor_count": {"$lte": {"$numberLong": "5"}}})""")
    )
    assertEquals(
      VenueR.where(_.mayor_count lte 5).q,
      pq("""db.venues.find({"mayor_count": {"$lte": {"$numberLong": "5"}}})""")
    )
    assertEquals(
      VenueR.where(_.mayor_count > 5).q,
      pq("""db.venues.find({"mayor_count": {"$gt": {"$numberLong": "5"}}})""")
    )
    assertEquals(
      VenueR.where(_.mayor_count gt 5).q,
      pq("""db.venues.find({"mayor_count": {"$gt": {"$numberLong": "5"}}})""")
    )
    assertEquals(
      VenueR.where(_.mayor_count >= 5).q,
      pq("""db.venues.find({"mayor_count": {"$gte": {"$numberLong": "5"}}})""")
    )
    assertEquals(
      VenueR.where(_.mayor_count gte 5).q,
      pq("""db.venues.find({"mayor_count": {"$gte": {"$numberLong": "5"}}})""")
    )
    assertEquals(
      VenueR.where(_.mayor_count between (3, 5)).q,
      pq(
        """db.venues.find({"mayor_count": {"$gte": {"$numberLong": "3"}, "$lte": {"$numberLong": "5"}}})"""
      )
    )
    assertEquals(
      VenueR.where(_.popularity < 4).q,
      pq("""db.venues.find({"popularity": {"$lt": {"$numberLong": "4"}}})""")
    )
    assertEquals(
      VenueClaimR.where(_.status neqs ClaimStatus.approved).q,
      pq("""db.venueclaims.find({"status": {"$ne": "Approved"}})""")
    )

    //TODO - EnumValue field
    /*assertEquals(    VenueClaimR.where(_.reason eqs RejectReason.tooManyClaims).q, pq("""db.venueclaims.find({"reason": 0})"""))
assertEquals(    VenueClaimR.where(_.reason eqs RejectReason.cheater).q, pq("""db.venueclaims.find({"reason": 1})"""))
assertEquals(    VenueClaimR.where(_.reason eqs RejectReason.wrongCode).q, pq("""db.venueclaims.find({"reason": 2})"""))
     */

    // comparison even when type information is unavailable
    /*def doLessThan[M <: CcMeta[M], T: BSONType](meta: M with CcMeta[M], f: M => Field[T, M], otherVal: T) =
    meta.where(r => f(r) < otherVal)
assertEquals(    doLessThan(Venue, (v: Venue) => v.mayor_count, 5L).q, pq("""db.venues.find({"mayor_count": {"$lt": 5}})"""))
     */

    // in,nin
    assertEquals(
      VenueR.where(_.legacyid in List(123L, 456L)).q,
      pq(
        """db.venues.find({"legId": {"$in": [{"$numberLong": "123"}, {"$numberLong": "456"}]}})"""
      )
    )
    assertEquals(
      VenueR.where(_.venuename nin List("Starbucks", "Whole Foods")).q,
      pq(
        """db.venues.find({"venuename": {"$nin": ["Starbucks", "Whole Foods"]}})"""
      )
    )
    assertEquals(
      VenueClaimR
        .where(_.status in List(ClaimStatus.approved, ClaimStatus.pending))
        .q,
      pq(
        """db.venueclaims.find({"status": {"$in": ["Approved", "Pending approval"]}})"""
      )
    )
    assertEquals(
      VenueClaimR
        .where(_.status nin List(ClaimStatus.approved, ClaimStatus.pending))
        .q,
      pq(
        """db.venueclaims.find({"status": {"$nin": ["Approved", "Pending approval"]}})"""
      )
    )

    assertEquals(
      VenueClaimR.where(_.venueid in List(ven1.id)).q,
      pq(
        ("""db.venueclaims.find({"vid": {"$in": [{"$oid": "%s"}]}})""" format oid1.toString)
      )
    )

    assertEquals(
      VenueClaimR.where(_.venueid nin List(ven1.id)).q,
      pq(
        ("""db.venueclaims.find({"vid": {"$nin": [{"$oid": "%s"}]}})""" format oid1.toString)
      )
    )

    // exists
    assertEquals(
      VenueR.where(_.id exists true).q,
      pq("""db.venues.find({"_id": {"$exists": true}})""")
    )

    // startsWith, regex

    assertEquals(
      VenueR.where(_.venuename startsWith "Starbucks").q,
      pq(
        """db.venues.find({"venuename": {"$regex": "^\\QStarbucks\\E", "$options": ""}})"""
      )
    )

    val p1 = Pattern.compile("Star.*")
    assertEquals(
      VenueR.where(_.venuename regexWarningNotIndexed p1).q,
      pq(
        """db.venues.find({"venuename": {"$regex": "Star.*", "$options": ""}})"""
      )
    )
    assertEquals(
      VenueR.where(_.venuename matches p1).q,
      pq(
        """db.venues.find({"venuename": {"$regex": "Star.*", "$options": ""}})"""
      )
    )
    val p2 =
      Pattern.compile("Star.*", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE)
    assertEquals(
      VenueR.where(_.venuename matches p2).q,
      pq(
        """db.venues.find({"venuename": {"$regex": "Star.*", "$options": "im"}})"""
      )
    )
//FIXME both regex AND nin
    assertEquals(
      VenueR
        .where(_.venuename matches p2)
        .and(_.venuename nin List("a", "b"))
        .q,
      pq(
        """db.venues.find({"venuename": {"$nin": ["a", "b"], "$regex": "Star.*", "$options": "im"}})"""
      )
    )

    // all, in, size, contains, at
    assertEquals(
      VenueR.where(_.tags eqs List("db", "ka")).q,
      pq("""db.venues.find({"tags": ["db", "ka"]})""")
    )
    assertEquals(
      VenueR.where(_.tags all List("db", "ka")).q,
      pq("""db.venues.find({"tags": {"$all": ["db", "ka"]}})""")
    )
    assertEquals(
      VenueR.where(_.tags in List("db", "ka")).q,
      pq("""db.venues.find({"tags": {"$in": ["db", "ka"]}})""")
    )
    assertEquals(
      VenueR.where(_.tags nin List("db", "ka")).q,
      pq("""db.venues.find({"tags": {"$nin": ["db", "ka"]}})""")
    )
    assertEquals(
      VenueR.where(_.tags neqs List("db", "ka")).q,
      pq("""db.venues.find({"tags": {"$ne": ["db", "ka"]}})""")
    )
    assertEquals(
      VenueR.where(_.tags matches "kara.*".r).q,
      pq("""db.venues.find({"tags": {"$regex": "kara.*", "$options": ""}})""")
    )
    assertEquals(
      VenueR.where(_.tags size 3).q,
      pq("""db.venues.find({"tags": {"$size": 3}})""")
    )
    assertEquals(
      VenueR.where(_.tags contains "karaoke").q,
      pq("""db.venues.find({"tags": "karaoke"})""")
    )
    assertEquals(
      VenueR.where(_.tags notcontains "karaoke").q,
      pq("""db.venues.find({"tags": {"$ne": "karaoke"}})""")
    )
    assertEquals(
      VenueR.where(_.popularity contains 3).q,
      pq("""db.venues.find({"popularity": {"$numberLong": "3"}})""")
    )
    assertEquals(
      VenueR.where(_.popularity at 0 eqs 3).q,
      pq("""db.venues.find({"popularity.0": {"$numberLong": "3"}})""")
    )
    assertEquals(
      VenueR.where(_.categories at 0 eqs oid).q,
      pq(
        """db.venues.find({"categories.0": {"$oid": "%s"}})""".format(
          oid.toString
        )
      )
    )
    assertEquals(
      VenueR.where(_.tags at 0 startsWith "kara").q,
      pq(
        """db.venues.find({"tags.0": {"$regex": "^\\Qkara\\E", "$options": ""}})"""
      )
    )
    // alternative syntax
    assertEquals(
      VenueR.where(_.tags idx 0 startsWith "kara").q,
      pq(
        """db.venues.find({"tags.0": {"$regex": "^\\Qkara\\E", "$options": ""}})"""
      )
    )
    assertEquals(
      VenueR.where(_.tags startsWith "kara").q,
      pq(
        """db.venues.find({"tags": {"$regex": "^\\Qkara\\E", "$options": ""}})"""
      )
    )
    assertEquals(
      VenueR.where(_.tags matches "k.*".r).q,
      pq("""db.venues.find({"tags": {"$regex": "k.*", "$options": ""}})""")
    )

    // maps
    assertEquals(
      TipR.where(_.counts at "foo" eqs 3).q,
      pq("""db.tips.find({"counts.foo": {"$numberLong": "3"}})""")
    )

    //TODO - by desing no LatLang near
    /*assertEquals(    VenueR.where(_.geolatlng near (39.0, -74.0, Degrees(0.2)))    .q, pq("""db.venues.find({"latlng": {"$near": [ 39.0, -74.0, 0.2 ]}})"""))
    assertEquals(    VenueR.where(_.geolatlng withinCircle(1.0, 2.0, Degrees(0.3))).q, pq("""db.venues.find({"latlng": {"$within": {"$center": [ [ 1.0, 2.0], 0.3]}}})"""))
    assertEquals(    VenueR.where(_.geolatlng withinBox(1.0, 2.0, 3.0, 4.0))       .q, pq("""db.venues.find({"latlng": {"$within": {"$box": [ [ 1.0, 2.0], [ 3.0, 4.0]]}}})"""))
    assertEquals(    VenueR.where(_.geolatlng eqs (45.0, 50.0))                    .q, pq("""db.venues.find({"latlng": [ 45.0, 50.0]})"""))
    assertEquals(    VenueR.where(_.geolatlng neqs (31.0, 23.0))                   .q, pq("""db.venues.find({"latlng": {"$ne": [ 31.0, 23.0]}})"""))
    assertEquals(    VenueR.where(_.geolatlng eqs LatLong(45.0, 50.0))             .q, pq("""db.venues.find({"latlng": [ 45.0, 50.0]})"""))
    assertEquals(    VenueR.where(_.geolatlng neqs LatLong(31.0, 23.0))            .q, pq("""db.venues.find({"latlng": {"$ne": [ 31.0, 23.0]}})"""))
    assertEquals(    VenueR.where(_.geolatlng nearSphere (39.0, -74.0, Radians(1.0))).q, pq("""db.venues.find({"latlng": {"$nearSphere": [ 39.0, -74.0], "$maxDistance": 1.0}})"""))
     */
    // ObjectId before, after, between
    assertEquals(
      VenueR.where(_.id before d2).q,
      pq(
        """db.venues.find({"_id": {"$lt": {"$oid": "%s"}}})""".format(
          oid2.toString
        )
      )
    )
    assertEquals(
      VenueR.where(_.id after d1).q,
      pq(
        """db.venues.find({"_id": {"$gt": {"$oid": "%s"}}})""".format(
          oid1.toString
        )
      )
    )
    assertEquals(
      VenueR.where(_.id between (d1, d2)).q,
      pq(
        """db.venues.find({"_id": {"$gt": {"$oid": "%s"}, "$lt": {"$oid": "%s"}}})"""
          .format(oid1.toString, oid2.toString)
      )
    )
    assertEquals(
      VenueR.where(_.id betweenR Tuple2(d1, d2)).q,
      pq(
        """db.venues.find({"_id": {"$gt": {"$oid": "%s"}, "$lt": {"$oid": "%s"}}})"""
          .format(oid1.toString, oid2.toString)
      )
    )

    // DateTime before, after, between
    assertEquals(
      VenueR.where(_.last_updated before d2).q,
      pq(
        """db.venues.find({"last_updated": {"$lt": {"$date": 1272758400000}}})"""
      )
    )
    assertEquals(
      VenueR.where(_.last_updated after d1).q,
      pq(
        """db.venues.find({"last_updated": {"$gt": {"$date": 1272672000000}}})"""
      )
    )
    assertEquals(
      VenueR.where(_.last_updated between (d1, d2)).q,
      pq(
        """db.venues.find({"last_updated": {"$gte": {"$date": 1272672000000}, "$lte": {"$date": 1272758400000}}})"""
      )
    )
    assertEquals(
      VenueR.where(_.last_updated between Tuple2(d1, d2)).q,
      pq(
        """db.venues.find({"last_updated": {"$gte": {"$date": 1272672000000}, "$lte": {"$date": 1272758400000}}})"""
      )
    )
    assertEquals(
      VenueR.where(_.last_updated eqs d1).q,
      pq("""db.venues.find({"last_updated": {"$date": 1272672000000}})""")
    )

    // Case class list field
    //assertEquals(    Comment.where(_.comments.unsafeField[Int]("z") contains 123).q, pq("""db.comments.find({"comments.z": 123})"""))
    //assertEquals(    Comment.where(_.comments.unsafeField[String]("comment") contains "hi").q, pq("""db.comments.find({"comments.comment": "hi"})"""))

    // BsonRecordField subfield queries
    assertEquals(
      VenueR.where(_.claims.subfield(_.status) contains ClaimStatus.approved).q,
      pq("""db.venues.find({"claims.status": "Approved"})""")
    )
    assertEquals(
      VenueR.where(_.claims.subfield(_.uid) between (1, 10)).q,
      pq(
        """db.venues.find({"claims.uid": {"$gte": {"$numberLong": "1"}, "$lte": {"$numberLong": "10"}}})"""
      )
    )
    assertEquals(
      VenueR.where(_.claims.subfield(_.date) between (d1, d2)).q,
      pq(
        """db.venues.find({"claims.date": {"$gte": {"$date": 1272672000000}, "$lte": {"$date": 1272758400000}}})"""
      )
    )
    assertEquals(
      VenueR.where(_.lastClaim.subfield(_.uid) eqs 123).q,
      pq("""db.venues.find({"lastClaim.uid": {"$numberLong": "123"}})""")
    )
    assertEquals(
      VenueR
        .where(_.claims.subfield(_.source.subfield(_.name)) contains "twitter")
        .q,
      pq("""db.venues.find({"claims.source.name": "twitter"})""")
    )

    // Enumeration list
    assertEquals(
      OAuthConsumerR
        .where(_.privileges contains ConsumerPrivilege.awardBadges)
        .q,
      pq("""db.oauthconsumers.find({"privileges": "Award badges"})""")
    )
    assertEquals(
      OAuthConsumerR
        .where(_.privileges at 0 eqs ConsumerPrivilege.awardBadges)
        .q,
      pq("""db.oauthconsumers.find({"privileges.0": "Award badges"})""")
    )

    // Field type
    assertEquals(
      VenueR.where(_.legacyid hastype MongoType.String).q,
      pq("""db.venues.find({"legId": {"$type": 2}})""")
    )

    // Modulus
    assertEquals(
      VenueR.where(_.legacyid mod (5, 1)).q,
      pq("""db.venues.find({"legId": {"$mod": [5, 1]}})""")
    )

    // compound queries
    assertEquals(
      VenueR.where(_.mayor eqs 1).and(_.tags contains "karaoke").q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "tags": "karaoke"})"""
      )
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).and(_.mayor_count eqs 5).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "mayor_count": {"$numberLong": "5"}})"""
      )
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).and(_.mayor_count lt 5).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "mayor_count": {"$lt": {"$numberLong": "5"}}})"""
      )
    )
    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .and(_.mayor_count gt 3)
        .and(_.mayor_count lt 5)
        .q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "mayor_count": {"$lt": {"$numberLong": "5"}, "$gt": {"$numberLong": "3"}}})"""
      )
    )

    // queries with no clauses
//assertEquals(    metaRecordToQueryBuilder(Venue).q, pq("db.venues.find({})"))
    assertEquals(
      VenueR.orderDesc(_.id).q,
      pq("""db.venues.find({}).sort({"_id": -1})""")
    )

    // ordered queries
    assertEquals(
      VenueR.where(_.mayor eqs 1).orderAsc(_.legacyid).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}).sort({"legId": 1})"""
      )
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).orderDesc(_.legacyid).andAsc(_.userId).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}).sort({"legId": -1, "userId": 1})"""
      )
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).orderDesc(_.lastClaim.subfield(_.date)).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}).sort({"lastClaim.date": -1})"""
      )
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).orderNaturalAsc.q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}).sort({"$natural": 1})"""
      )
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).orderNaturalDesc.q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}).sort({"$natural": -1})"""
      )
    )

    // select queries
    assertEquals(
      VenueR.where(_.mayor eqs 1).select(_.id).q,
      pq("""db.venues.find({"mayor": {"$numberLong": "1"}}, {"_id": 1})""")
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).select(_.id, _.legacyid).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"_id": 1, "legId": 1})"""
      )
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).select(_.id, _.legacyid, _.userId).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"_id": 1, "legId": 1, "userId": 1})"""
      )
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).select(_.legacyid, _.userId, _.mayor).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "mayor": 1, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .select(_.legacyid, _.userId, _.mayor, _.mayor_count)
        .q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "mayor": 1, "mayor_count": 1, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .select(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed)
        .q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "mayor": 1, "mayor_count": 1, "closed": 1, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .select(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed, _.tags)
        .q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "mayor": 1, "mayor_count": 1, "closed": 1, "tags": 1, "_id": 0})"""
      )
    )

    // select case queries
    assertEquals(
      VenueR.where(_.mayor eqs 1).selectCase(_.legacyid, V1).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).selectCase(_.legacyid, _.userId, V2).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .selectCase(_.legacyid, _.userId, _.mayor, V3)
        .q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "mayor": 1, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .selectCase(_.legacyid, _.userId, _.mayor, _.mayor_count, V4)
        .q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "mayor": 1, "mayor_count": 1, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .selectCase(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed, V5)
        .q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "mayor": 1, "mayor_count": 1, "closed": 1, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .selectCase(
          _.legacyid,
          _.userId,
          _.mayor,
          _.mayor_count,
          _.closed,
          _.tags,
          V6
        )
        .q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "mayor": 1, "mayor_count": 1, "closed": 1, "tags": 1, "_id": 0})"""
      )
    )

    // select subfields
    assertEquals(
      TipR.where(_.legacyid eqs 1).select(_.counts at "foo").q,
      pq(
        """db.tips.find({"legid": {"$numberLong": "1"}}, {"counts.foo": 1, "_id": 0})"""
      )
    )
    //assertEquals(    VenueR.where(_.legacyid eqs 1).select(_.geolatlng.unsafeField[Double]("lat")).q, pq("""db.venues.find({"legId": {"$numberLong": "1"}}, {"latlng.lat": 1})"""))
    assertEquals(
      VenueR.where(_.legacyid eqs 1).select(_.lastClaim.subselect(_.status)).q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}}, {"lastClaim.status": 1, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).select(_.claims.subselect(_.uid)).q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}}, {"claims.uid": 1, "_id": 0})"""
      )
    )

    // select slice
    assertEquals(
      VenueR.where(_.legacyid eqs 1).select(_.tags).q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}}, {"tags": 1, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).select(_.tags.slice(4)).q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}}, {"tags": {"$slice": 4}, "_id": 0})"""
      )
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).select(_.tags.slice(4, 7)).q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}}, {"tags": {"$slice": [4, 7]}, "_id": 0})"""
      )
    )

    // select $
    assertEquals(
      VenueR.where(_.legacyid eqs 1).select(_.tags.$$).q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}}, {"tags.$": 1, "_id": 0})"""
      )
    )

    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .and(
          _.claims elemMatch (_.status eqs ClaimStatus.approved,
          _.uid gt 2097)
        )
        .q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}, "claims": {"$elemMatch": {"uid": {"$gt": {"$numberLong": "2097"}}, "status": "Approved"}}})"""
      )
    )

    // TODO: case class list fields
    //assertEquals(     Comment.select(_.comments.unsafeField[Long]("userid")).q, pq("""db.venues.find({}, {"comments.userid": 1})"""))

    // out of order and doesn't screw up earlier params
    assertEquals(
      VenueR.limit(10).where(_.mayor eqs 1).q,
      pq("""db.venues.find({"mayor": {"$numberLong": "1"}}).limit(10)""")
    )
    assertEquals(
      VenueR.orderDesc(_.id).and(_.mayor eqs 1).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}).sort({"_id": -1})"""
      )
    )
    assertEquals(
      VenueR.orderDesc(_.id).skip(3).and(_.mayor eqs 1).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}}).sort({"_id": -1}).skip(3)"""
      )
    )

    // Scan should be the same as and/where
    assertEquals(
      VenueR.where(_.mayor eqs 1).scan(_.tags contains "karaoke").q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "tags": "karaoke"})"""
      )
    )
    assertEquals(
      VenueR.scan(_.mayor eqs 1).and(_.mayor_count eqs 5).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "mayor_count": {"$numberLong": "5"}})"""
      )
    )
    assertEquals(
      VenueR.scan(_.mayor eqs 1).scan(_.mayor_count lt 5).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "mayor_count": {"$lt": {"$numberLong": "5"}}})"""
      )
    )

    // limit, limitOpt, skip, skipOpt
    assertEquals(
      VenueR.where(_.mayor eqs 1).limit(10).q,
      pq("""db.venues.find({"mayor": {"$numberLong": "1"}}).limit(10)""")
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).limitOpt(Some(10)).q,
      pq("""db.venues.find({"mayor": {"$numberLong": "1"}}).limit(10)""")
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).limitOpt(None).q,
      pq("""db.venues.find({"mayor": {"$numberLong": "1"}})""")
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).skip(10).q,
      pq("""db.venues.find({"mayor": {"$numberLong": "1"}}).skip(10)""")
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).skipOpt(Some(10)).q,
      pq("""db.venues.find({"mayor": {"$numberLong": "1"}}).skip(10)""")
    )
    assertEquals(
      VenueR.where(_.mayor eqs 1).skipOpt(None).q,
      pq("""db.venues.find({"mayor": {"$numberLong": "1"}})""")
    )

    // raw query clauses
    assertEquals(
      VenueR.where(_.mayor eqs 1).raw(_.add("$where", "this.a > 3")).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "$where": "this.a > 3"})"""
      )
    )

    // $not clauses
    assertEquals(
      VenueR.scan(_.mayor eqs 1).scan(_.mayor_count not (_ lt 5)).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "mayor_count": {"$not": {"$lt": {"$numberLong": "5"}}}})"""
      )
    )
    assertEquals(
      VenueR
        .scan(_.mayor eqs 1)
        .scan(_.mayor_count not (_ lt 5))
        .and(_.mayor_count not (_ gt 6))
        .q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "mayor_count": {"$not": {"$gt": {"$numberLong": "6"}, "$lt": {"$numberLong": "5"}}}})"""
      )
    )
    assertEquals(
      VenueR
        .scan(_.mayor eqs 1)
        .scan(_.mayor_count not (_ lt 5))
        .and(_.mayor_count gt 3)
        .q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "1"}, "mayor_count": {"$gt": {"$numberLong": "3"}, "$not": {"$lt": {"$numberLong": "5"}}}})"""
      )
    )
    assertEquals(
      VenueR.scan(_.id not (_ before d1)).q,
      pq(
        """db.venues.find({"_id": {"$not": {"$lt": {"$oid": "%s"}}}})""".format(
          oid1.toString
        )
      )
    )
    assertEquals(
      VenueR.scan(_.last_updated not (_ between (d1, d2))).q,
      pq(
        """db.venues.find({"last_updated": {"$not": {"$gte": {"$date": 1272672000000}, "$lte": {"$date": 1272758400000}}}})"""
      )
    )
    assertEquals(
      VenueR.scan(_.tags not (_ in List("a", "b"))).q,
      pq("""db.venues.find({"tags": {"$not": {"$in": ["a", "b"]}}})""")
    )
    assertEquals(
      VenueR.scan(_.tags not (_ size 0)).q,
      pq("""db.venues.find({"tags": {"$not": {"$size": 0}}})""")
    )
    assertEquals(
      VenueR.scan(_.popularity at 0 not (_ lt 0)).q,
      pq(
        """db.venues.find({"popularity.0": {"$not": {"$lt": {"$numberLong": "0"}}}})"""
      )
    )

    assertEquals(
      VenueR.search("test name", Some("pl"), Some(true), Some(false)).q,
      pq(
        """db.venues.find({"$text": {"$search": "test name", "$language": "pl", "$caseSensitive": true, "$diacriticSensitive": false}})"""
      )
    )
    assertEquals(
      VenueR.search("test name", None, Some(true), Some(false)).q,
      pq(
        """db.venues.find({"$text": {"$search": "test name", "$caseSensitive": true, "$diacriticSensitive": false}})"""
      )
    )
    assertEquals(
      VenueR.search("test name", Some("pl"), None, Some(false)).q,
      pq(
        """db.venues.find({"$text": {"$search": "test name", "$language": "pl", "$diacriticSensitive": false}})"""
      )
    )
    assertEquals(
      VenueR.search("test name", Some("pl"), Some(true), None).q,
      pq(
        """db.venues.find({"$text": {"$search": "test name", "$language": "pl", "$caseSensitive": true}})"""
      )
    )
    assertEquals(
      VenueR.search("test name", Some("pl"), None, None).q,
      pq(
        """db.venues.find({"$text": {"$search": "test name", "$language": "pl"}})"""
      )
    )
    assertEquals(
      VenueR.search("test name", None, None, Some(true)).q,
      pq(
        """db.venues.find({"$text": {"$search": "test name", "$diacriticSensitive": true}})"""
      )
    )
    assertEquals(
      VenueR.search("test name", None, Some(true), None).q,
      pq(
        """db.venues.find({"$text": {"$search": "test name", "$caseSensitive": true}})"""
      )
    )
    assertEquals(
      VenueR.search("test name", None, None, None).q,
      pq("""db.venues.find({"$text": {"$search": "test name"}})""")
    )

    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .modify(_.claims.`$[]`.subfield(_.status) setTo ClaimStatus.approved)
        .q,
      pq(
        """db.venues.update({"mayor": {"$numberLong": "1"}}, {"$set": {"claims.$[].status": "Approved"}}, false, false)"""
      )
    )
  }

  test("ModifyQueryShouldProduceACorrectJSONQueryString") {
    val d1 = LocalDateTime.of(2010, 5, 1, 0, 0, 0, 0) //, DateTimeZone.UTC)

    val query = """db.venues.update({"legId": {"$numberLong": "1"}}, """
    val suffix = ", false, false)"
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.venuename setTo "fshq").q,
      pq(query + """{"$set": {"venuename": "fshq"}}""" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.mayor_count setTo 3).q,
      pq(query + """{"$set": {"mayor_count": {"$numberLong": "3"}}}""" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.mayor_count unset).q,
      pq(query + """{"$unset": {"mayor_count": 1}}""" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.mayor_count setTo Some(3L)).q,
      pq(query + """{"$set": {"mayor_count": {"$numberLong": "3"}}}""" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.mayor_count setTo None).q,
      pq(query + """{"$unset": {"mayor_count": 1}}""" + suffix)
    )

    // Numeric
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.mayor_count inc 3).q,
      pq(query + """{"$inc": {"mayor_count": 3}}""" + suffix)
    )
    //assertEquals(    VenueR.where(_.legacyid eqs 1).modify(_.geolatlng.unsafeField[Double]("lat") inc 0.5).q, pq(query + """{"$inc": {"latlng.lat": 0.5}}""" + suffix))

    // Enumeration
    val query2 = """db.venueclaims.update({"uid": {"$numberLong": "1"}}, """
    assertEquals(
      VenueClaimR
        .where(_.userId eqs 1)
        .modify(_.status setTo ClaimStatus.approved)
        .q,
      pq(query2 + """{"$set": {"status": "Approved"}}""" + suffix)
    )

    // Calendar
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.last_updated setTo d1).q,
      pq(
        query + """{"$set": {"last_updated": {"$date": 1272672000000}}}""" + suffix
      )
    )

    // LatLong
    val ll = LatLong(37.4, -73.9)
    //assertEquals(    VenueR.where(_.legacyid eqs 1).modify(_.geolatlng setTo ll).q, pq(query + """{"$set": {"latlng": [ 37.4, -73.9]}}""" + suffix))

    // Lists
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.popularity setTo List(5)).q,
      pq(
        query + """{"$set": {"popularity": [{"$numberLong": "5"}]}}""" + suffix
      )
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.popularity push 5).q,
      pq(query + """{"$push": {"popularity": {"$numberLong": "5"}}}""" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.tags pushAll List("a", "b")).q,
      pq(query + """{"$pushAll": {"tags": ["a", "b"]}}""" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.tags addToSet "a").q,
      pq(query + """{"$addToSet": {"tags": "a"}}""" + suffix)
    )
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(_.popularity addToSet List(1L, 2L))
        .q,
      pq(
        query + """{"$addToSet": {"popularity": {"$each": [{"$numberLong": "1"}, {"$numberLong": "2"}]}}}""" + suffix
      )
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.popularity push List(1L, 2L)).q,
      pq(
        query + """{"$push": {"popularity": {"$each": [{"$numberLong": "1"}, {"$numberLong": "2"}]}}}""" + suffix
      )
    )
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(_.popularity push (List(1L, 2L), 3))
        .q,
      pq(
        query + """{"$push": {"popularity": {"$each": [{"$numberLong": "1"}, {"$numberLong": "2"}], "$slice": 3}}}""" + suffix
      )
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.tags.popFirst).q,
      pq(query + """{"$pop": {"tags": -1}}""" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.tags.popLast).q,
      pq(query + """{"$pop": {"tags": 1}}""" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.tags pull "a").q,
      pq(query + """{"$pull": {"tags": "a"}}""" + suffix)
    )
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(_.popularity pullAll List(2L, 3L))
        .q,
      pq(
        query + """{"$pullAll": {"popularity": [{"$numberLong": "2"}, {"$numberLong": "3"}]}}""" + suffix
      )
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.popularity at 0 inc 1).q,
      pq(query + """{"$inc": {"popularity.0": 1}}""" + suffix)
    )
    // alternative syntax
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.popularity idx 0 inc 1).q,
      pq(query + """{"$inc": {"popularity.0": 1}}""" + suffix)
    )

    // Enumeration list
    assertEquals(
      OAuthConsumerR
        .modify(_.privileges addToSet ConsumerPrivilege.awardBadges)
        .q,
      pq(
        """db.oauthconsumers.update({}, {"$addToSet": {"privileges": "Award badges"}}""" + suffix
      )
    )

    // BsonRecordField and BsonRecordListField with nested Enumeration
    val claims = List(
      VenueClaimBson(1L, ClaimStatus.approved, Option(SourceBson("", "")), d1)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.claims setTo claims).q,
      pq(
        query + """{"$set": {"claims": [{"date": {"$date": 1272672000000}, "source": {"url": "", "name": ""}, "status": "Approved", "uid": {"$numberLong": "1"}}]}}""" + suffix
      )
    )

    //query + """{"$set": {"claims": [ {"status": "Approved", "uid": 1, "source": {"name": "", "url": ""}, "date": {"$date": "2010-05-01T00:00:00.000Z"}}]}}""" + suffix
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.lastClaim setTo claims.head).q,
      pq(
        query + """{"$set": {"lastClaim": {"date": {"$date": 1272672000000}, "source": {"url": "", "name": ""}, "status": "Approved", "uid": {"$numberLong": "1"}}}}""" + suffix
      )
    )
    //"""{"$set": {"lastClaim": {"status": "Approved", "uid": 1, "source": {"name": "", "url": ""}, "date": {"$date": "2010-05-01T00:00:00.000Z"}}}}""".stripMargin + suffix

    // Map
    val m = Map("foo" -> 1L)
    val query3 = """db.tips.update({"legid": {"$numberLong": "1"}}, """
    assertEquals(
      TipR.where(_.legacyid eqs 1).modify(_.counts setTo m).q,
      pq(
        query3 + """{"$set": {"counts": {"foo": {"$numberLong": "1"}}}}""" + suffix
      )
    )
    assertEquals(
      TipR.where(_.legacyid eqs 1).modify(_.counts at "foo" setTo 3).q,
      pq(query3 + """{"$set": {"counts.foo": {"$numberLong": "3"}}}""" + suffix)
    )
    assertEquals(
      TipR.where(_.legacyid eqs 1).modify(_.counts at "foo" inc 5).q,
      pq(query3 + """{"$inc": {"counts.foo": 5}}""" + suffix)
    )
    assertEquals(
      TipR.where(_.legacyid eqs 1).modify(_.counts at "foo" unset).q,
      pq(query3 + """{"$unset": {"counts.foo": 1}}""" + suffix)
    )
    assertEquals(
      TipR
        .where(_.legacyid eqs 1)
        .modify(_.counts setTo Map("foo" -> 3L, "bar" -> 5L))
        .q,
      pq(
        query3 + """{"$set": {"counts": {"bar": {"$numberLong": "5"}, "foo": {"$numberLong": "3"}}}}""" + suffix
      )
    )

    // Multiple updates
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(_.venuename setTo "fshq")
        .and(_.mayor_count setTo 3)
        .q,
      pq(
        query + """{"$set": {"mayor_count": {"$numberLong": "3"}, "venuename": "fshq"}}""" + suffix
      )
    )
    //WARNING - order of $set / $inc differes in scala 2.12 / 2.13
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(_.venuename setTo "fshq")
        .and(_.mayor_count inc 1)
        .q,
      pq(
        query + """{"$inc": {"mayor_count": 1}, "$set": {"venuename": "fshq"}}""" + suffix
      )
    )
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(_.venuename setTo "fshq")
        .and(_.mayor_count setTo 3)
        .and(_.mayor_count inc 1)
        .q,
      pq(
        query + """{"$set": {"mayor_count": {"$numberLong": "3"}, "venuename": "fshq"}, "$inc": {"mayor_count": 1}}""" + suffix
      )
    )
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(_.popularity addToSet 3)
        .and(_.tags addToSet List("a", "b"))
        .q,
      pq(
        query + """{"$addToSet": {"tags": {"$each": ["a", "b"]}, "popularity": {"$numberLong": "3"}}}""" + suffix
      )
    )

    // Noop query
    assertEquals(
      VenueR.where(_.legacyid eqs 1).noop().q,
      pq(query + "{}" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).noop().modify(_.venuename setTo "fshq").q,
      pq(query + """{"$set": {"venuename": "fshq"}}""" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).noop().and(_.venuename setTo "fshq").q,
      pq(query + """{"$set": {"venuename": "fshq"}}""" + suffix)
    )

    // $bit
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.mayor_count bitAnd 3).q,
      pq(query + """{"$bit": {"mayor_count": {"and": 3}}}""" + suffix)
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.mayor_count bitOr 3).q,
      pq(query + """{"$bit": {"mayor_count": {"or": 3}}}""" + suffix)
    )

    // $rename
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.venuename rename "vn").q,
      pq(query + """{"$rename": {"venuename": "vn"}}""" + suffix)
    )

    // $setOnInsert
    assertEquals(
      VenueR.where(_.legacyid eqs 1).modify(_.venuename setOnInsertTo "fshq").q,
      pq(query + """{"$setOnInsert": {"venuename": "fshq"}}""" + suffix)
    )

    // pullWhere
    /*
    object tags extends MongoListField[Venue, String](this)
    object popularity extends MongoListField[Venue, Long](this)
    object categories extends MongoListField[Venue, ObjectId](this)
    object claims extends BsonRecordListField(this, VenueClaimBson)
     */
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(_.tags pullWhere (_ startsWith "prefix"))
        .q,
      pq(
        query + """{"$pull": {"tags": {"$regex": "^\\Qprefix\\E", "$options": ""}}}""" + suffix
      )
    )
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(_.popularity pullWhere (_ gt 2))
        .q,
      pq(
        query + """{"$pull": {"popularity": {"$gt": {"$numberLong": "2"}}}}""" + suffix
      )
    )
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(_.popularity pullWhere (_ gt 2, _ lt 5))
        .q,
      pq(
        query + """{"$pull": {"popularity": {"$gt": {"$numberLong": "2"}, "$lt": {"$numberLong": "5"}}}}""" + suffix
      )
    )
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .modify(
          _.claims pullObjectWhere (_.status eqs ClaimStatus.approved,
          _.uid eqs 2097)
        )
        .q,
      pq(
        query + """{"$pull": {"claims": {"uid": {"$numberLong": "2097"}, "status": "Approved"}}}""" + suffix
      )
    )
  }

  test("ProduceACorrectSignatureString") {
    val d1 = LocalDateTime.of(2010, 5, 1, 0, 0, 0, 0)
    val d2 = LocalDateTime.of(2010, 5, 2, 0, 0, 0, 0)
    val oid = tag[Venue](new ObjectId)

    // basic ops
    assertEquals(
      VenueR.where(_.mayor eqs 1).signature(),
      """db.venues.find({"mayor": 0})"""
    )
    assertEquals(
      VenueR.where(_.venuename eqs "Starbucks").signature(),
      """db.venues.find({"venuename": 0})"""
    )
    assertEquals(
      VenueR.where(_.closed eqs true).signature(),
      """db.venues.find({"closed": 0})"""
    )
    assertEquals(
      VenueR.where(_.id eqs oid).signature(),
      """db.venues.find({"_id": 0})"""
    )
    assertEquals(
      VenueClaimR.where(_.status eqs ClaimStatus.approved).signature(),
      """db.venueclaims.find({"status": 0})"""
    )
    assertEquals(
      VenueR.where(_.mayor_count gte 5).signature(),
      """db.venues.find({"mayor_count": {"$gte": 0}})"""
    )
    assertEquals(
      VenueClaimR.where(_.status neqs ClaimStatus.approved).signature(),
      """db.venueclaims.find({"status": {"$ne": 0}})"""
    )
    assertEquals(
      VenueR.where(_.legacyid in List(123L, 456L)).signature(),
      """db.venues.find({"legId": {"$in": 0}})"""
    )
    assertEquals(
      VenueR.where(_.id exists true).signature(),
      """db.venues.find({"_id": {"$exists": 0}})"""
    )
    assertEquals(
      VenueR.where(_.venuename startsWith "Starbucks").signature(),
      """db.venues.find({"venuename": {"$regex": 0, "$options": 0}})"""
    )

    // list
    assertEquals(
      VenueR.where(_.tags all List("db", "ka")).signature(),
      """db.venues.find({"tags": {"$all": 0}})"""
    )
    assertEquals(
      VenueR.where(_.tags in List("db", "ka")).signature(),
      """db.venues.find({"tags": {"$in": 0}})"""
    )
    assertEquals(
      VenueR.where(_.tags size 3).signature(),
      """db.venues.find({"tags": {"$size": 0}})"""
    )
    assertEquals(
      VenueR.where(_.tags contains "karaoke").signature(),
      """db.venues.find({"tags": 0})"""
    )
    assertEquals(
      VenueR.where(_.popularity contains 3).signature(),
      """db.venues.find({"popularity": 0})"""
    )
    assertEquals(
      VenueR.where(_.popularity at 0 eqs 3).signature(),
      """db.venues.find({"popularity.0": 0})"""
    )
    assertEquals(
      VenueR.where(_.categories at 0 eqs oid).signature(),
      """db.venues.find({"categories.0": 0})"""
    )
    assertEquals(
      VenueR.where(_.tags at 0 startsWith "kara").signature(),
      """db.venues.find({"tags.0": {"$regex": 0, "$options": 0}})"""
    )
    assertEquals(
      VenueR.where(_.tags idx 0 startsWith "kara").signature(),
      """db.venues.find({"tags.0": {"$regex": 0, "$options": 0}})"""
    )

    // map
    assertEquals(
      TipR.where(_.counts at "foo" eqs 3).signature(),
      """db.tips.find({"counts.foo": 0})"""
    )

    // near
    /*assertEquals(    VenueR.where(_.geolatlng near (39.0, -74.0, Degrees(0.2)))    .signature(), """db.venues.find({"latlng": {"$near": 0}})""")
    assertEquals(    VenueR.where(_.geolatlng withinCircle(1.0, 2.0, Degrees(0.3))).signature(), """db.venues.find({"latlng": {"$within": {"$center": 0}}})""")
    assertEquals(    VenueR.where(_.geolatlng withinBox(1.0, 2.0, 3.0, 4.0))       .signature(), """db.venues.find({"latlng": {"$within": {"$box": 0}}})""")
    assertEquals(    VenueR.where(_.geolatlng eqs (45.0, 50.0)).signature(), """db.venues.find({"latlng": 0})""")
    assertEquals(    VenueR.where(_.geolatlng nearSphere (39.0, -74.0, Radians(1.0)))    .signature(), """db.venues.find({"latlng": {"$nearSphere": 0, "$maxDistance": 0}})""")
     */
    // id, date range
    assertEquals(
      VenueR.where(_.id before d2).signature(),
      """db.venues.find({"_id": {"$lt": 0}})"""
    )
    assertEquals(
      VenueR.where(_.last_updated before d2).signature(),
      """db.venues.find({"last_updated": {"$lt": 0}})"""
    )

    // Case class list field
    /*
assertEquals(    Comment.where(_.comments.unsafeField[Int]("z") contains 123)          .signature(), """db.comments.find({"comments.z": 0})""")
assertEquals(    Comment.where(_.comments.unsafeField[String]("comment") contains "hi").signature(), """db.comments.find({"comments.comment": 0})""")
     */

    // Enumeration list
    assertEquals(
      OAuthConsumerR
        .where(_.privileges contains ConsumerPrivilege.awardBadges)
        .signature(),
      """db.oauthconsumers.find({"privileges": 0})"""
    )
    assertEquals(
      OAuthConsumerR
        .where(_.privileges at 0 eqs ConsumerPrivilege.awardBadges)
        .signature(),
      """db.oauthconsumers.find({"privileges.0": 0})"""
    )

    // Field type
    assertEquals(
      VenueR.where(_.legacyid hastype MongoType.String).signature(),
      """db.venues.find({"legId": {"$type": 0}})"""
    )

    // Modulus
    assertEquals(
      VenueR.where(_.legacyid mod (5, 1)).signature(),
      """db.venues.find({"legId": {"$mod": 0}})"""
    )

    // compound queries
    assertEquals(
      VenueR.where(_.mayor eqs 1).and(_.tags contains "karaoke").signature(),
      """db.venues.find({"mayor": 0, "tags": 0})"""
    )
    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .and(_.mayor_count gt 3)
        .and(_.mayor_count lt 5)
        .signature(),
      """db.venues.find({"mayor": 0, "mayor_count": {"$lt": 0, "$gt": 0}})"""
    )

    // queries with no clauses
    assertEquals(ccMetaToQueryBuilder(VenueR).signature(), "db.venues.find({})")
    assertEquals(
      VenueR.orderDesc(_.id).signature(),
      """db.venues.find({}).sort({"_id": -1})"""
    )

    // ordered queries
    assertEquals(
      VenueR.where(_.mayor eqs 1).orderAsc(_.legacyid).signature(),
      """db.venues.find({"mayor": 0}).sort({"legId": 1})"""
    )
    assertEquals(
      VenueR
        .where(_.mayor eqs 1)
        .orderDesc(_.legacyid)
        .andAsc(_.userId)
        .signature(),
      """db.venues.find({"mayor": 0}).sort({"legId": -1, "userId": 1})"""
    )

    // select queries
    assertEquals(
      VenueR.where(_.mayor eqs 1).select(_.legacyid).signature(),
      """db.venues.find({"mayor": 0})"""
    )

    // Scan should be the same as and/where
    assertEquals(
      VenueR.where(_.mayor eqs 1).scan(_.tags contains "karaoke").signature(),
      """db.venues.find({"mayor": 0, "tags": 0})"""
    )

    // or queries
    assertEquals(
      VenueR.where(_.mayor eqs 1).or(_.where(_.id eqs oid)).signature(),
      """db.venues.find({"mayor": 0, "$or": [{"_id": 0}]})"""
    )
  }

  test("FindAndModifyQueryShouldProduceACorrectJSONQueryString") {
    assertEquals(
      VenueR.where(_.legacyid eqs 1).findAndModify(_.venuename setTo "fshq").q,
      pq(
        """db.venues.findAndModify({ query: {"legId": {"$numberLong": "1"}}, update: {"$set": {"venuename": "fshq"}}, new: false, upsert: false })"""
      )
    )
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .orderAsc(_.popularity)
        .findAndModify(_.venuename setTo "fshq")
        .q,
      pq(
        """db.venues.findAndModify({ query: {"legId": {"$numberLong": "1"}}, sort: {"popularity": 1}, update: {"$set": {"venuename": "fshq"}}, new: false, upsert: false })"""
      )
    )
    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .select(_.mayor, _.closed)
        .findAndModify(_.venuename setTo "fshq")
        .q,
      pq(
        """db.venues.findAndModify({ query: {"legId": {"$numberLong": "1"}}, update: {"$set": {"venuename": "fshq"}}, new: false, fields: {"mayor": 1, "closed": 1, "_id": 0}, upsert: false })"""
      )
    )
  }

  test("OrQueryShouldProduceACorrectJSONQueryString") {
    // Simple $or
    assertEquals(
      VenueR.or(_.where(_.legacyid eqs 1), _.where(_.mayor eqs 2)).q,
      pq(
        """db.venues.find({"$or": [{"legId": {"$numberLong": "1"}}, {"mayor": {"$numberLong": "2"}}]})"""
      )
    )

    // Compound $or
    assertEquals(
      VenueR
        .where(_.tags size 0)
        .or(_.where(_.legacyid eqs 1), _.where(_.mayor eqs 2))
        .q,
      pq(
        """db.venues.find({"tags": {"$size": 0}, "$or": [{"legId": {"$numberLong": "1"}}, {"mayor": {"$numberLong": "2"}}]})"""
      )
    )

    // $or with additional "and" clauses
    assertEquals(
      VenueR
        .where(_.tags size 0)
        .or(
          _.where(_.legacyid eqs 1).and(_.closed eqs true),
          _.where(_.mayor eqs 2)
        )
        .q,
      pq(
        """db.venues.find({"tags": {"$size": 0}, "$or": [{"legId": {"$numberLong": "1"}, "closed": true}, {"mayor": {"$numberLong": "2"}}]})"""
      )
    )

    // Nested $or
    assertEquals(
      VenueR
        .or(
          _.where(_.legacyid eqs 1)
            .or(_.where(_.closed eqs true), _.where(_.closed exists false)),
          _.where(_.mayor eqs 2)
        )
        .q,
      pq(
        """db.venues.find({"$or": [{"legId": {"$numberLong": "1"}, "$or": [{"closed": true}, {"closed": {"$exists": false}}]}, {"mayor": {"$numberLong": "2"}}]})"""
      )
    )

    // $or with modify
    assertEquals(
      VenueR
        .or(_.where(_.legacyid eqs 1), _.where(_.mayor eqs 2))
        .modify(_.userId setTo 1)
        .q,
      pq(
        """db.venues.update({"$or": [{"legId": {"$numberLong": "1"}}, {"mayor": {"$numberLong": "2"}}]}, {"$set": {"userId": {"$numberLong": "1"}}}, false, false)"""
      )
    )

    // $or with optional where clause
    assertEquals(
      VenueR
        .or(_.where(_.legacyid eqs 1), _.whereOpt(None)(_.mayor eqs _))
        .modify(_.userId setTo 1)
        .q,
      pq(
        """db.venues.update({"$or": [{"legId": {"$numberLong": "1"}}]}, {"$set": {"userId": {"$numberLong": "1"}}}, false, false)"""
      )
    )

    assertEquals(
      VenueR
        .or(_.where(_.legacyid eqs 1), _.whereOpt(Some(2))(_.mayor eqs _))
        .modify(_.userId setTo 1)
        .q,
      pq(
        """db.venues.update({"$or": [{"legId": {"$numberLong": "1"}}, {"mayor": {"$numberLong": "2"}}]}, {"$set": {"userId": {"$numberLong": "1"}}}, false, false)"""
      )
    )

    // OrQuery syntax
    val q1 = VenueR.where(_.legacyid eqs 1)
    val q2 = VenueR.where(_.legacyid eqs 2)
    assertEquals(
      OrQuery(q1, q2).q,
      pq(
        """db.venues.find({"$or": [{"legId": {"$numberLong": "1"}}, {"legId": {"$numberLong": "2"}}]})"""
      )
    )
    assertEquals(
      OrQuery(q1, q2).and(_.mayor eqs 0).q,
      pq(
        """db.venues.find({"mayor": {"$numberLong": "0"}, "$or": [{"legId": {"$numberLong": "1"}}, {"legId": {"$numberLong": "2"}}]})"""
      )
    )
    assertEquals(
      OrQuery(
        q1,
        q2.or(_.where(_.closed eqs true), _.where(_.closed exists false))
      ).q,
      pq(
        """db.venues.find({"$or": [{"legId": {"$numberLong": "1"}}, {"legId": {"$numberLong": "2"}, "$or": [{"closed": true}, {"closed": {"$exists": false}}]}]})"""
      )
    )
  }

  test("Hints") {
    assertEquals(
      VenueR.where(_.legacyid eqs 1).hint(VenueR.idIdx).q,
      pq("""db.venues.find({"legId": {"$numberLong": "1"}}).hint({"_id": 1})""")
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).hint(VenueR.legIdx).q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}}).hint({"legId": -1})"""
      )
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).hint(VenueR.legIdIdx).q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}}).hint({"legId": 1, "_id": -1})"""
      )
    )
    //no support for geo indexes yet in test
    //VenueR.where(_.legacyid eqs 1).hint(VenueR.geoIdx).toString()       must_== """db.venues.find({"legId": 1}).hint({"latlng": "2d"})"""
    //assertEquals(    VenueR.where(_.legacyid eqs 1).hint(VenueR.geoCustomIdx).q, pq("""db.venues.find({"legId": 1}).hint({"latlng": "custom", "tags": 1})"""))

    //optional hints
    assertEquals(
      VenueR.where(_.legacyid eqs 1).hintOpt(Some(VenueR.idIdx)).q,
      pq("""db.venues.find({"legId": {"$numberLong": "1"}}).hint({"_id": 1})""")
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).hintOpt(Some(VenueR.legIdx)).q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}}).hint({"legId": -1})"""
      )
    )
    assertEquals(
      VenueR.where(_.legacyid eqs 1).hintOpt(None).q,
      pq("""db.venues.find({"legId": {"$numberLong": "1"}})""")
    )
  }

  test("DollarSelector") {

    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .and(_.claims.subfield(_.uid) contains 2)
        .modify(_.claims.$.subfield(_.status) setTo ClaimStatus.approved)
        .q,
      pq(
        """db.venues.update({"legId": {"$numberLong": "1"}, "claims.uid": {"$numberLong": "2"}}, {"$set": {"claims.$.status": "Approved"}}, false, false)"""
      )
    )

    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .and(_.tags contains "sometag")
        .modify(_.tags.$ setTo "othertag")
        .q,
      pq(
        """db.venues.update({"legId": {"$numberLong": "1"}, "tags": "sometag"}, {"$set": {"tags.$": "othertag"}}, false, false)"""
      )
    )

    assertEquals(
      VenueR
        .where(_.legacyid eqs 1)
        .and(_.tags contains "sometag")
        .select(_.tags.$$)
        .q,
      pq(
        """db.venues.find({"legId": {"$numberLong": "1"}, "tags": "sometag"}, {"tags.$": 1, "_id": 0})"""
      )
    )
  }
  /*
      @Test
      def testWhereOpt {
        val someId = Some(1L)
        val noId: Option[Long] = None
        val someList = Some(List(1L, 2L))
        val noList: Option[List[Long]] = None

        // whereOpt
assertEquals(        VenueR.whereOpt(someId)(_.legacyid eqs _).q, pq("""db.venues.find({"legId": 1})"""))
assertEquals(        VenueR.whereOpt(noId)(_.legacyid eqs _).q, pq("""db.venues.find({})"""))
assertEquals(        VenueR.whereOpt(someId)(_.legacyid eqs _).and(_.mayor eqs 2).q, pq("""db.venues.find({"legId": 1, "mayor": 2})"""))
assertEquals(        VenueR.whereOpt(noId)(_.legacyid eqs _).and(_.mayor eqs 2).q, pq("""db.venues.find({"mayor": 2})"""))

        // whereOpt: lists
assertEquals(        VenueR.whereOpt(someList)(_.legacyid in _).q, pq("""db.venues.find({"legId": {"$in": [ 1, 2 ]}})"""))
assertEquals(        VenueR.whereOpt(noList)(_.legacyid in _).q, pq("""db.venues.find({})"""))

        // whereOpt: enum
        val someEnum = Some(VenueStatus.open)
        val noEnum: Option[VenueStatus.type#Value] = None
assertEquals(        VenueR.whereOpt(someEnum)(_.status eqs _).q, pq("""db.venues.find({"status": "Open"})"""))
assertEquals(        VenueR.whereOpt(noEnum)(_.status eqs _).q, pq("""db.venues.find({})"""))

        // whereOpt: date
        val someDate = Some(new DateTime(2010, 5, 1, 0, 0, 0, 0, DateTimeZone.UTC))
        val noDate: Option[DateTime] = None
assertEquals(        VenueR.whereOpt(someDate)(_.last_updated after _).q, pq("""db.venues.find({"last_updated": {"$gt": {"$date": "2010-05-01T00:00:00.000Z"}}})"""))
assertEquals(        VenueR.whereOpt(noDate)(_.last_updated after _).q, pq("""db.venues.find({})"""))

        // andOpt
assertEquals(        VenueR.where(_.mayor eqs 2).andOpt(someId)(_.legacyid eqs _).q, pq("""db.venues.find({"mayor": 2, "legid": 1})"""))
assertEquals(        VenueR.where(_.mayor eqs 2).andOpt(noId)(_.legacyid eqs _).q, pq("""db.venues.find({"mayor": 2})"""))

        // scanOpt
assertEquals(        VenueR.scanOpt(someId)(_.legacyid eqs _).q, pq("""db.venues.find({"legId": 1})"""))
assertEquals(        VenueR.scanOpt(noId)(_.legacyid eqs _).q, pq("""db.venues.find({})"""))
assertEquals(        VenueR.scanOpt(someId)(_.legacyid eqs _).and(_.mayor eqs 2).q, pq("""db.venues.find({"legId": 1, "mayor": 2})"""))
assertEquals(        VenueR.scanOpt(noId)(_.legacyid eqs _).and(_.mayor eqs 2).q, pq("""db.venues.find({"mayor": 2})"""))

        // iscanOpt
assertEquals(        VenueR.iscanOpt(someId)(_.legacyid eqs _).q, pq("""db.venues.find({"legId": 1})"""))
assertEquals(        VenueR.iscanOpt(noId)(_.legacyid eqs _).q, pq("""db.venues.find({})"""))
assertEquals(        VenueR.iscanOpt(someId)(_.legacyid eqs _).and(_.mayor eqs 2).q, pq("""db.venues.find({"legId": 1, "mayor": 2})"""))
assertEquals(        VenueR.iscanOpt(noId)(_.legacyid eqs _).and(_.mayor eqs 2).q, pq("""db.venues.find({"mayor": 2})"""))

        // modify
        val q = VenueR.where(_.legacyid eqs 1)
        val prefix = """db.venues.update({"legid": 1}, """
        val suffix = ", false, false)"

assertEquals(        q.modifyOpt(someId)(_.legacyid setTo _).q, pq(prefix + """{"$set": {"legid": 1}}""" + suffix))
assertEquals(        q.modifyOpt(noId)(_.legacyid setTo _).q, pq(prefix + """{}""" + suffix))
assertEquals(        q.modifyOpt(someEnum)(_.status setTo _).q, pq(prefix + """{"$set": {"status": "Open"}}""" + suffix))
assertEquals(        q.modifyOpt(noEnum)(_.status setTo _).q, pq(prefix + """{}""" + suffix))
     }

      @Test
      def testShardKey {
assertEquals(        LikeR.where(_.checkin eqs 123).q, pq("""db.likes.find({"checkin": 123})"""))
assertEquals(        LikeR.where(_.userid eqs 123).q, pq("""db.likes.find({"userid": 123})"""))
assertEquals(        LikeR.where(_.userid eqs 123).allShards.q, pq("""db.likes.find({"userid": 123})"""))
assertEquals(        LikeR.where(_.userid eqs 123).allShards.noop().q, pq("""db.likes.update({"userid": 123}, {}, false, false)"""))
assertEquals(        LikeR.withShardKey(_.userid eqs 123).q, pq("""db.likes.find({"userid": 123})"""))
assertEquals(        LikeR.withShardKey(_.userid in List(123L, 456L)).q, pq("""db.likes.find({"userid": {"$in": [ 123, 456]}})"""))
assertEquals(        LikeR.withShardKey(_.userid eqs 123).and(_.checkin eqs 1).q, pq("""db.likes.find({"userid": 123, "checkin": 1})"""))
assertEquals(        LikeR.where(_.checkin eqs 1).withShardKey(_.userid eqs 123).q, pq("""db.likes.find({"checkin": 1, "userid": 123})"""))
     }

      @Test
      def testCommonSuperclassForPhantomTypes {
        def maybeLimit(legid: Long, limitOpt: Option[Int]) = {
          limitOpt match {
            case Some(limit) => VenueR.where(_.legacyid eqs legid).limit(limit)
            case None => VenueR.where(_.legacyid eqs legid)
         }
       }

assertEquals(        maybeLimit(1, None).q, pq("""db.venues.find({"legId": 1})"""))
assertEquals(        maybeLimit(1, Some(5)).q, pq("""db.venues.find({"legId": 1}).limit(5)"""))
     }

      @Test
      def testSetReadPreference: Unit = {
        type Q = Query[Venue, Venue, _]

        VenueR.where(_.mayor eqs 2).asInstanceOf[Q].readPreference must_== None
        VenueR.where(_.mayor eqs 2).setReadPreference(ReadPreference.secondary).asInstanceOf[Q].readPreference must_== Some(ReadPreference.secondary)
        VenueR.where(_.mayor eqs 2).setReadPreference(ReadPreference.primary).asInstanceOf[Q].readPreference must_== Some(ReadPreference.primary)
        VenueR.where(_.mayor eqs 2).setReadPreference(ReadPreference.secondary).setReadPreference(ReadPreference.primary).asInstanceOf[Q].readPreference must_== Some(ReadPreference.primary)
     }

   */

  test("uuidTest") {
    val u = UUID.randomUUID()
    //it is ok it just compiles
    UuidCcR.where(_.id eqs u)
    UuidCcR.where(_.dt gte Instant.now())
  }

  test("NullEq") {
    val q1 = UuidCcR.where(_.dt.eqsNull).asDBObject
    val q2 = UuidCcR.where(_.dt.neqsNull).asDBObject
    assertEquals(q1.toString, """{"i": null}""")
    assertEquals(q2.toString, """{"i": {"$ne": null}}""")
  }
}
