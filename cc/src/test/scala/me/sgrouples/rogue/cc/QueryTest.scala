package me.sgrouples.rogue.cc

// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.

import java.time.{ Instant, LocalDateTime, ZoneOffset }
import java.util.UUID

import com.mongodb.ReadPreference
import io.fsq.rogue._
import CcRogue._
import java.util.regex.Pattern
import javax.xml.crypto.dsig.Transform

import com.mongodb.util.{ JSON, JSONSerializers }
import io.fsq.field.Field
import me.sgrouples.rogue.CClassListField
import me.sgrouples.rogue.cc.Metas._
import org.bson.{ BSON, BsonDateTime, BsonDocument, Transformer }
import org.bson.types._
import org.junit._
import org.specs2.matcher.JUnitMustMatchers
import shapeless.tag
import shapeless.tag.@@

class QueryTest extends JUnitMustMatchers {
  // to make queries printable

  BSON.addEncodingHook(classOf[BsonDocument], new Transformer() {
    override def transform(o: scala.Any): AnyRef = {
      val js = o.asInstanceOf[BsonDocument].toJson
      //println(js)
      JSON.parse(js)
    }
  })

  @Test
  def testProduceACorrectJSONQueryString {
    val d1 = LocalDateTime.of(2010, 5, 1, 0, 0, 0, 0)
    val d2 = LocalDateTime.of(2010, 5, 2, 0, 0, 0, 0)
    val oid1 = ObjectId.createFromLegacyFormat(d1.toEpochSecond(ZoneOffset.UTC).toInt, 0, 0)
    val oid2 = ObjectId.createFromLegacyFormat(d2.toEpochSecond(ZoneOffset.UTC).toInt, 0, 0)
    val oid = new ObjectId
    case class Ven1(id: ObjectId @@ Venue)
    val ven1 = Ven1(tag[Venue](oid1))

    // eqs
    VenueR.where(_.mayor eqs 1).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } })"""
    VenueR.where(_.venuename eqs "Starbucks").toString() must_== """db.venues.find({ "venuename" : "Starbucks" })"""
    VenueR.where(_.closed eqs true).toString() must_== """db.venues.find({ "closed" : true })"""
    VenueR.where(_.id eqs tag[Venue](oid)).toString() must_== ("""db.venues.find({ "_id" : { "$oid" : "%s" } })""" format oid.toString)
    VenueClaimR.where(_.status eqs ClaimStatus.approved).toString() must_== """db.venueclaims.find({ "status" : "Approved" })"""

    VenueClaimR.where(_.venueid eqs tag[Venue](oid)).toString() must_== ("""db.venueclaims.find({ "vid" : { "$oid" : "%s" } })""" format oid.toString)
    VenueClaimR.where(_.venueid eqs tag[Venue](ven1.id)).toString() must_== ("""db.venueclaims.find({ "vid" : { "$oid" : "%s" } })""" format oid1.toString)

    // neq,lt,gt
    VenueR.where(_.mayor_count neqs 5).toString() must_== """db.venues.find({ "mayor_count" : { "$ne" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.mayor_count < 5).toString() must_== """db.venues.find({ "mayor_count" : { "$lt" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.mayor_count lt 5).toString() must_== """db.venues.find({ "mayor_count" : { "$lt" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.mayor_count <= 5).toString() must_== """db.venues.find({ "mayor_count" : { "$lte" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.mayor_count lte 5).toString() must_== """db.venues.find({ "mayor_count" : { "$lte" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.mayor_count > 5).toString() must_== """db.venues.find({ "mayor_count" : { "$gt" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.mayor_count gt 5).toString() must_== """db.venues.find({ "mayor_count" : { "$gt" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.mayor_count >= 5).toString() must_== """db.venues.find({ "mayor_count" : { "$gte" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.mayor_count gte 5).toString() must_== """db.venues.find({ "mayor_count" : { "$gte" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.mayor_count between (3, 5)).toString() must_== """db.venues.find({ "mayor_count" : { "$gte" : { "$numberLong" : "3" }, "$lte" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.popularity < 4).toString() must_== """db.venues.find({ "popularity" : { "$lt" : { "$numberLong" : "4" } } })"""
    VenueClaimR.where(_.status neqs ClaimStatus.approved).toString() must_== """db.venueclaims.find({ "status" : { "$ne" : "Approved" } })"""

    //TODO - EnumValue field
    /*VenueClaimR.where(_.reason eqs RejectReason.tooManyClaims).toString() must_== """db.venueclaims.find({ "reason" : 0 })"""
    VenueClaimR.where(_.reason eqs RejectReason.cheater).toString() must_== """db.venueclaims.find({ "reason" : 1 })"""
    VenueClaimR.where(_.reason eqs RejectReason.wrongCode).toString() must_== """db.venueclaims.find({ "reason" : 2})"""
*/

    // comparison even when type information is unavailable
    /*def doLessThan[M <: CcMeta[M], T: BSONType](meta: M with CcMeta[M], f: M => Field[T, M], otherVal: T) =
    meta.where(r => f(r) < otherVal)
    doLessThan(Venue, (v: Venue) => v.mayor_count, 5L).toString() must_== """db.venues.find({ "mayor_count" : { "$lt" : 5}})"""
*/

    // in,nin
    VenueR.where(_.legacyid in List(123L, 456L)).toString() must_== """db.venues.find({ "legId" : { "$in" : [{ "$numberLong" : "123" }, { "$numberLong" : "456" }] } })"""
    VenueR.where(_.venuename nin List("Starbucks", "Whole Foods")).toString() must_== """db.venues.find({ "venuename" : { "$nin" : ["Starbucks", "Whole Foods"] } })"""
    VenueClaimR.where(_.status in List(ClaimStatus.approved, ClaimStatus.pending)).toString() must_== """db.venueclaims.find({ "status" : { "$in" : ["Approved", "Pending approval"] } })"""
    VenueClaimR.where(_.status nin List(ClaimStatus.approved, ClaimStatus.pending)).toString() must_== """db.venueclaims.find({ "status" : { "$nin" : ["Approved", "Pending approval"] } })"""

    VenueClaimR.where(_.venueid in List(ven1.id)).toString() must_== ("""db.venueclaims.find({ "vid" : { "$in" : [{ "$oid" : "%s" }] } })""" format oid1.toString)

    VenueClaimR.where(_.venueid nin List(ven1.id)).toString() must_== ("""db.venueclaims.find({ "vid" : { "$nin" : [{ "$oid" : "%s" }] } })""" format oid1.toString)

    // exists
    VenueR.where(_.id exists true).toString() must_== """db.venues.find({ "_id" : { "$exists" : true } })"""

    // startsWith, regex
    VenueR.where(_.venuename startsWith "Starbucks").toString() must_== """db.venues.find({ "venuename" : { "$regex" : "^\\QStarbucks\\E", "$options" : "" } })"""
    val p1 = Pattern.compile("Star.*")
    VenueR.where(_.venuename regexWarningNotIndexed p1).toString() must_== """db.venues.find({ "venuename" : { "$regex" : "Star.*", "$options" : "" } })"""
    VenueR.where(_.venuename matches p1).toString() must_== """db.venues.find({ "venuename" : { "$regex" : "Star.*", "$options" : "" } })"""
    val p2 = Pattern.compile("Star.*", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE)
    VenueR.where(_.venuename matches p2).toString() must_== """db.venues.find({ "venuename" : { "$regex" : "Star.*", "$options" : "im" } })"""
    VenueR.where(_.venuename matches p2).and(_.venuename nin List("a", "b")).toString() must_== """db.venues.find({ "venuename" : { "$nin" : ["a", "b"], "$regex" : "Star.*", "$options" : "im" } })"""

    // all, in, size, contains, at
    VenueR.where(_.tags eqs List("db", "ka")).toString() must_== """db.venues.find({ "tags" : ["db", "ka"] })"""
    VenueR.where(_.tags all List("db", "ka")).toString() must_== """db.venues.find({ "tags" : { "$all" : ["db", "ka"] } })"""
    VenueR.where(_.tags in List("db", "ka")).toString() must_== """db.venues.find({ "tags" : { "$in" : ["db", "ka"] } })"""
    VenueR.where(_.tags nin List("db", "ka")).toString() must_== """db.venues.find({ "tags" : { "$nin" : ["db", "ka"] } })"""
    VenueR.where(_.tags neqs List("db", "ka")).toString() must_== """db.venues.find({ "tags" : { "$ne" : ["db", "ka"] } })"""
    VenueR.where(_.tags matches "kara.*".r).toString() must_== """db.venues.find({ "tags" : { "$regex" : "kara.*", "$options" : "" } })"""
    VenueR.where(_.tags size 3).toString() must_== """db.venues.find({ "tags" : { "$size" : 3 } })"""
    VenueR.where(_.tags contains "karaoke").toString() must_== """db.venues.find({ "tags" : "karaoke" })"""
    VenueR.where(_.tags notcontains "karaoke").toString() must_== """db.venues.find({ "tags" : { "$ne" : "karaoke" } })"""
    VenueR.where(_.popularity contains 3).toString() must_== """db.venues.find({ "popularity" : { "$numberLong" : "3" } })"""
    VenueR.where(_.popularity at 0 eqs 3).toString() must_== """db.venues.find({ "popularity.0" : { "$numberLong" : "3" } })"""
    VenueR.where(_.categories at 0 eqs oid).toString() must_== """db.venues.find({ "categories.0" : { "$oid" : "%s" } })""".format(oid.toString)
    VenueR.where(_.tags at 0 startsWith "kara").toString() must_== """db.venues.find({ "tags.0" : { "$regex" : "^\\Qkara\\E", "$options" : "" } })"""
    // alternative syntax
    VenueR.where(_.tags idx 0 startsWith "kara").toString() must_== """db.venues.find({ "tags.0" : { "$regex" : "^\\Qkara\\E", "$options" : "" } })"""
    VenueR.where(_.tags startsWith "kara").toString() must_== """db.venues.find({ "tags" : { "$regex" : "^\\Qkara\\E", "$options" : "" } })"""
    VenueR.where(_.tags matches "k.*".r).toString() must_== """db.venues.find({ "tags" : { "$regex" : "k.*", "$options" : "" } })"""

    // maps
    TipR.where(_.counts at "foo" eqs 3).toString() must_== """db.tips.find({ "counts.foo" : { "$numberLong" : "3" } })"""

    //TODO - by desing no LatLang near
    /*VenueR.where(_.geolatlng near (39.0, -74.0, Degrees(0.2)))    .toString() must_== """db.venues.find({ "latlng" : { "$near" : [ 39.0, -74.0, 0.2 ] }})"""
    VenueR.where(_.geolatlng withinCircle(1.0, 2.0, Degrees(0.3))).toString() must_== """db.venues.find({ "latlng" : { "$within" : { "$center" : [ [ 1.0, 2.0], 0.3]}}})"""
    VenueR.where(_.geolatlng withinBox(1.0, 2.0, 3.0, 4.0))       .toString() must_== """db.venues.find({ "latlng" : { "$within" : { "$box" : [ [ 1.0, 2.0], [ 3.0, 4.0]]}}})"""
    VenueR.where(_.geolatlng eqs (45.0, 50.0))                    .toString() must_== """db.venues.find({ "latlng" : [ 45.0, 50.0]})"""
    VenueR.where(_.geolatlng neqs (31.0, 23.0))                   .toString() must_== """db.venues.find({ "latlng" : { "$ne" : [ 31.0, 23.0]}})"""
    VenueR.where(_.geolatlng eqs LatLong(45.0, 50.0))             .toString() must_== """db.venues.find({ "latlng" : [ 45.0, 50.0]})"""
    VenueR.where(_.geolatlng neqs LatLong(31.0, 23.0))            .toString() must_== """db.venues.find({ "latlng" : { "$ne" : [ 31.0, 23.0]}})"""
    VenueR.where(_.geolatlng nearSphere (39.0, -74.0, Radians(1.0))).toString() must_== """db.venues.find({ "latlng" : { "$nearSphere" : [ 39.0, -74.0], "$maxDistance" : 1.0 }})"""
*/
    // ObjectId before, after, between
    VenueR.where(_.id before d2).toString() must_== """db.venues.find({ "_id" : { "$lt" : { "$oid" : "%s" } } })""".format(oid2.toString)
    VenueR.where(_.id after d1).toString() must_== """db.venues.find({ "_id" : { "$gt" : { "$oid" : "%s" } } })""".format(oid1.toString)
    VenueR.where(_.id between (d1, d2)).toString() must_== """db.venues.find({ "_id" : { "$gt" : { "$oid" : "%s" }, "$lt" : { "$oid" : "%s" } } })""".format(oid1.toString, oid2.toString)
    VenueR.where(_.id betweenR Tuple2(d1, d2)).toString() must_== """db.venues.find({ "_id" : { "$gt" : { "$oid" : "%s" }, "$lt" : { "$oid" : "%s" } } })""".format(oid1.toString, oid2.toString)

    // DateTime before, after, between
    VenueR.where(_.last_updated before d2).toString() must_== """db.venues.find({ "last_updated" : { "$lt" : { "$date" : 1272758400000 } } })"""
    VenueR.where(_.last_updated after d1).toString() must_== """db.venues.find({ "last_updated" : { "$gt" : { "$date" : 1272672000000 } } })"""
    VenueR.where(_.last_updated between (d1, d2)).toString() must_== """db.venues.find({ "last_updated" : { "$gte" : { "$date" : 1272672000000 }, "$lte" : { "$date" : 1272758400000 } } })"""
    VenueR.where(_.last_updated between Tuple2(d1, d2)).toString() must_== """db.venues.find({ "last_updated" : { "$gte" : { "$date" : 1272672000000 }, "$lte" : { "$date" : 1272758400000 } } })"""
    VenueR.where(_.last_updated eqs d1).toString() must_== """db.venues.find({ "last_updated" : { "$date" : 1272672000000 } })"""

    // Case class list field
    //Comment.where(_.comments.unsafeField[Int]("z") contains 123).toString() must_== """db.comments.find({ "comments.z" : 123 })"""
    //Comment.where(_.comments.unsafeField[String]("comment") contains "hi").toString() must_== """db.comments.find({ "comments.comment" : "hi" })"""

    // BsonRecordField subfield queries
    VenueR.where(_.claims.subfield(_.status) contains ClaimStatus.approved).toString() must_== """db.venues.find({ "claims.status" : "Approved" })"""
    VenueR.where(_.claims.subfield(_.uid) between (1, 10)).toString() must_== """db.venues.find({ "claims.uid" : { "$gte" : { "$numberLong" : "1" }, "$lte" : { "$numberLong" : "10" } } })"""
    VenueR.where(_.claims.subfield(_.date) between (d1, d2)).toString() must_== """db.venues.find({ "claims.date" : { "$gte" : { "$date" : 1272672000000 }, "$lte" : { "$date" : 1272758400000 } } })"""
    VenueR.where(_.lastClaim.subfield(_.uid) eqs 123).toString() must_== """db.venues.find({ "lastClaim.uid" : { "$numberLong" : "123" } })"""
    VenueR.where(_.claims.subfield(_.source.subfield(_.name)) contains "twitter").toString() must_== """db.venues.find({ "claims.source.name" : "twitter" })"""

    // Enumeration list
    OAuthConsumerR.where(_.privileges contains ConsumerPrivilege.awardBadges).toString() must_== """db.oauthconsumers.find({ "privileges" : "Award badges" })"""
    OAuthConsumerR.where(_.privileges at 0 eqs ConsumerPrivilege.awardBadges).toString() must_== """db.oauthconsumers.find({ "privileges.0" : "Award badges" })"""

    // Field type
    VenueR.where(_.legacyid hastype MongoType.String).toString() must_== """db.venues.find({ "legId" : { "$type" : 2 } })"""

    // Modulus
    VenueR.where(_.legacyid mod (5, 1)).toString() must_== """db.venues.find({ "legId" : { "$mod" : [5, 1] } })"""

    // compound queries
    VenueR.where(_.mayor eqs 1).and(_.tags contains "karaoke").toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "tags" : "karaoke" })"""
    VenueR.where(_.mayor eqs 1).and(_.mayor_count eqs 5).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "mayor_count" : { "$numberLong" : "5" } })"""
    VenueR.where(_.mayor eqs 1).and(_.mayor_count lt 5).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "mayor_count" : { "$lt" : { "$numberLong" : "5" } } })"""
    VenueR.where(_.mayor eqs 1).and(_.mayor_count gt 3).and(_.mayor_count lt 5).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "mayor_count" : { "$lt" : { "$numberLong" : "5" }, "$gt" : { "$numberLong" : "3" } } })"""

    // queries with no clauses
    //metaRecordToQueryBuilder(Venue).toString() must_== "db.venues.find({ })"
    VenueR.orderDesc(_.id).toString() must_== """db.venues.find({ }).sort({ "_id" : -1 })"""

    // ordered queries
    VenueR.where(_.mayor eqs 1).orderAsc(_.legacyid).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).sort({ "legId" : 1 })"""
    VenueR.where(_.mayor eqs 1).orderDesc(_.legacyid).andAsc(_.userId).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).sort({ "legId" : -1, "userId" : 1 })"""
    VenueR.where(_.mayor eqs 1).orderDesc(_.lastClaim.subfield(_.date)).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).sort({ "lastClaim.date" : -1 })"""
    VenueR.where(_.mayor eqs 1).orderNaturalAsc.toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).sort({ "$natural" : 1 })"""
    VenueR.where(_.mayor eqs 1).orderNaturalDesc.toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).sort({ "$natural" : -1 })"""

    // select queries
    VenueR.where(_.mayor eqs 1).select(_.id).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "_id" : 1 })"""
    VenueR.where(_.mayor eqs 1).select(_.id, _.legacyid).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "_id" : 1, "legId" : 1 })"""
    VenueR.where(_.mayor eqs 1).select(_.id, _.legacyid, _.userId).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "_id" : 1, "legId" : 1, "userId" : 1 })"""
    VenueR.where(_.mayor eqs 1).select(_.legacyid, _.userId, _.mayor).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "legId" : 1, "userId" : 1, "mayor" : 1 })"""
    VenueR.where(_.mayor eqs 1).select(_.legacyid, _.userId, _.mayor, _.mayor_count).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "legId" : 1, "userId" : 1, "mayor" : 1, "mayor_count" : 1 })"""
    VenueR.where(_.mayor eqs 1).select(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "legId" : 1, "userId" : 1, "mayor" : 1, "mayor_count" : 1, "closed" : 1 })"""
    VenueR.where(_.mayor eqs 1).select(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed, _.tags).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "legId" : 1, "userId" : 1, "mayor" : 1, "mayor_count" : 1, "closed" : 1, "tags" : 1 })"""

    // select case queries
    VenueR.where(_.mayor eqs 1).selectCase(_.legacyid, V1).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "legId" : 1 })"""
    VenueR.where(_.mayor eqs 1).selectCase(_.legacyid, _.userId, V2).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "legId" : 1, "userId" : 1 })"""
    VenueR.where(_.mayor eqs 1).selectCase(_.legacyid, _.userId, _.mayor, V3).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "legId" : 1, "userId" : 1, "mayor" : 1 })"""
    VenueR.where(_.mayor eqs 1).selectCase(_.legacyid, _.userId, _.mayor, _.mayor_count, V4).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "legId" : 1, "userId" : 1, "mayor" : 1, "mayor_count" : 1 })"""
    VenueR.where(_.mayor eqs 1).selectCase(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed, V5).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "legId" : 1, "userId" : 1, "mayor" : 1, "mayor_count" : 1, "closed" : 1 })"""
    VenueR.where(_.mayor eqs 1).selectCase(_.legacyid, _.userId, _.mayor, _.mayor_count, _.closed, _.tags, V6).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }, { "legId" : 1, "userId" : 1, "mayor" : 1, "mayor_count" : 1, "closed" : 1, "tags" : 1 })"""

    // select subfields
    TipR.where(_.legacyid eqs 1).select(_.counts at "foo").toString() must_== """db.tips.find({ "legid" : { "$numberLong" : "1" } }, { "counts.foo" : 1 })"""
    //VenueR.where(_.legacyid eqs 1).select(_.geolatlng.unsafeField[Double]("lat")).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }, { "latlng.lat" : 1 })"""
    VenueR.where(_.legacyid eqs 1).select(_.lastClaim.subselect(_.status)).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }, { "lastClaim.status" : 1 })"""
    VenueR.where(_.legacyid eqs 1).select(_.claims.subselect(_.uid)).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }, { "claims.uid" : 1 })"""

    // select slice
    VenueR.where(_.legacyid eqs 1).select(_.tags).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }, { "tags" : 1 })"""
    VenueR.where(_.legacyid eqs 1).select(_.tags.slice(4)).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }, { "tags" : { "$slice" : 4 } })"""
    VenueR.where(_.legacyid eqs 1).select(_.tags.slice(4, 7)).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }, { "tags" : { "$slice" : [4, 7] } })"""

    // select $
    VenueR.where(_.legacyid eqs 1).select(_.tags.$$).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }, { "tags.$" : 1 })"""

    VenueR.where(_.legacyid eqs 1)
      .and(_.claims elemMatch (_.status eqs ClaimStatus.approved,
        _.uid gt 2097)).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" }, "claims" : { "$elemMatch" : { "uid" : { "$gt" : { "$numberLong" : "2097" } }, "status" : "Approved" } } })"""

    // TODO: case class list fields
    // Comment.select(_.comments.unsafeField[Long]("userid")).toString() must_== """db.venues.find({ }, { "comments.userid" : 1 })"""

    // out of order and doesn't screw up earlier params
    VenueR.limit(10).where(_.mayor eqs 1).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).limit(10)"""
    VenueR.orderDesc(_.id).and(_.mayor eqs 1).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).sort({ "_id" : -1 })"""
    VenueR.orderDesc(_.id).skip(3).and(_.mayor eqs 1).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).sort({ "_id" : -1 }).skip(3)"""

    // Scan should be the same as and/where
    VenueR.where(_.mayor eqs 1).scan(_.tags contains "karaoke").toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "tags" : "karaoke" })"""
    VenueR.scan(_.mayor eqs 1).and(_.mayor_count eqs 5).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "mayor_count" : { "$numberLong" : "5" } })"""
    VenueR.scan(_.mayor eqs 1).scan(_.mayor_count lt 5).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "mayor_count" : { "$lt" : { "$numberLong" : "5" } } })"""

    // limit, limitOpt, skip, skipOpt
    VenueR.where(_.mayor eqs 1).limit(10).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).limit(10)"""
    VenueR.where(_.mayor eqs 1).limitOpt(Some(10)).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).limit(10)"""
    VenueR.where(_.mayor eqs 1).limitOpt(None).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } })"""
    VenueR.where(_.mayor eqs 1).skip(10).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).skip(10)"""
    VenueR.where(_.mayor eqs 1).skipOpt(Some(10)).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } }).skip(10)"""
    VenueR.where(_.mayor eqs 1).skipOpt(None).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" } })"""

    // raw query clauses
    VenueR.where(_.mayor eqs 1).raw(_.add("$where", "this.a > 3")).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "$where" : "this.a > 3" })"""

    // $not clauses
    VenueR.scan(_.mayor eqs 1).scan(_.mayor_count not (_ lt 5)).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "mayor_count" : { "$not" : { "$lt" : { "$numberLong" : "5" } } } })"""
    VenueR.scan(_.mayor eqs 1).scan(_.mayor_count not (_ lt 5)).and(_.mayor_count not (_ gt 6)).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "mayor_count" : { "$not" : { "$gt" : { "$numberLong" : "6" }, "$lt" : { "$numberLong" : "5" } } } })"""
    VenueR.scan(_.mayor eqs 1).scan(_.mayor_count not (_ lt 5)).and(_.mayor_count gt 3).toString() must_== """db.venues.find({ "mayor" : { "$numberLong" : "1" }, "mayor_count" : { "$gt" : { "$numberLong" : "3" }, "$not" : { "$lt" : { "$numberLong" : "5" } } } })"""
    VenueR.scan(_.id not (_ before d1)).toString() must_== """db.venues.find({ "_id" : { "$not" : { "$lt" : { "$oid" : "%s" } } } })""".format(oid1.toString)
    VenueR.scan(_.last_updated not (_ between (d1, d2))).toString() must_== """db.venues.find({ "last_updated" : { "$not" : { "$gte" : { "$date" : 1272672000000 }, "$lte" : { "$date" : 1272758400000 } } } })"""
    VenueR.scan(_.tags not (_ in List("a", "b"))).toString() must_== """db.venues.find({ "tags" : { "$not" : { "$in" : ["a", "b"] } } })"""
    VenueR.scan(_.tags not (_ size 0)).toString() must_== """db.venues.find({ "tags" : { "$not" : { "$size" : 0 } } })"""
    VenueR.scan(_.popularity at 0 not (_ lt 0)).toString() must_== """db.venues.find({ "popularity.0" : { "$not" : { "$lt" : { "$numberLong" : "0" } } } })"""
  }

  @Test
  def testModifyQueryShouldProduceACorrectJSONQueryString {
    val d1 = LocalDateTime.of(2010, 5, 1, 0, 0, 0, 0) //, DateTimeZone.UTC)

    val query = """db.venues.update({ "legId" : { "$numberLong" : "1" } }, """
    val suffix = ", false, false)"
    VenueR.where(_.legacyid eqs 1).modify(_.venuename setTo "fshq").toString() must_== query + """{ "$set" : { "venuename" : "fshq" } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.mayor_count setTo 3).toString() must_== query + """{ "$set" : { "mayor_count" : { "$numberLong" : "3" } } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.mayor_count unset).toString() must_== query + """{ "$unset" : { "mayor_count" : 1 } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.mayor_count setTo Some(3L)).toString() must_== query + """{ "$set" : { "mayor_count" : { "$numberLong" : "3" } } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.mayor_count setTo None).toString() must_== query + """{ "$unset" : { "mayor_count" : 1 } }""" + suffix

    // Numeric
    VenueR.where(_.legacyid eqs 1).modify(_.mayor_count inc 3).toString() must_== query + """{ "$inc" : { "mayor_count" : 3 } }""" + suffix
    //VenueR.where(_.legacyid eqs 1).modify(_.geolatlng.unsafeField[Double]("lat") inc 0.5).toString() must_== query + """{ "$inc" : { "latlng.lat" : 0.5}}""" + suffix

    // Enumeration
    val query2 = """db.venueclaims.update({ "uid" : { "$numberLong" : "1" } }, """
    VenueClaimR.where(_.userId eqs 1).modify(_.status setTo ClaimStatus.approved).toString() must_== query2 + """{ "$set" : { "status" : "Approved" } }""" + suffix

    // Calendar
    VenueR.where(_.legacyid eqs 1).modify(_.last_updated setTo d1).toString() must_== query + """{ "$set" : { "last_updated" : { "$date" : 1272672000000 } } }""" + suffix

    // LatLong
    val ll = LatLong(37.4, -73.9)
    //VenueR.where(_.legacyid eqs 1).modify(_.geolatlng setTo ll).toString() must_== query + """{ "$set" : { "latlng" : [ 37.4, -73.9]}}""" + suffix

    // Lists
    VenueR.where(_.legacyid eqs 1).modify(_.popularity setTo List(5)).toString() must_== query + """{ "$set" : { "popularity" : [{ "$numberLong" : "5" }] } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.popularity push 5).toString() must_== query + """{ "$push" : { "popularity" : { "$numberLong" : "5" } } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.tags pushAll List("a", "b")).toString() must_== query + """{ "$pushAll" : { "tags" : ["a", "b"] } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.tags addToSet "a").toString() must_== query + """{ "$addToSet" : { "tags" : "a" } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.popularity addToSet List(1L, 2L)).toString() must_== query + """{ "$addToSet" : { "popularity" : { "$each" : [{ "$numberLong" : "1" }, { "$numberLong" : "2" }] } } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.popularity push List(1L, 2L)).toString() must_== query + """{ "$push" : { "popularity" : { "$each" : [{ "$numberLong" : "1" }, { "$numberLong" : "2" }] } } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.popularity push (List(1L, 2L), 3)).toString() must_== query + """{ "$push" : { "popularity" : { "$each" : [{ "$numberLong" : "1" }, { "$numberLong" : "2" }], "$slice" : 3 } } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.tags popFirst).toString() must_== query + """{ "$pop" : { "tags" : -1 } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.tags popLast).toString() must_== query + """{ "$pop" : { "tags" : 1 } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.tags pull "a").toString() must_== query + """{ "$pull" : { "tags" : "a" } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.popularity pullAll List(2L, 3L)).toString() must_== query + """{ "$pullAll" : { "popularity" : [{ "$numberLong" : "2" }, { "$numberLong" : "3" }] } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.popularity at 0 inc 1).toString() must_== query + """{ "$inc" : { "popularity.0" : 1 } }""" + suffix
    // alternative syntax
    VenueR.where(_.legacyid eqs 1).modify(_.popularity idx 0 inc 1).toString() must_== query + """{ "$inc" : { "popularity.0" : 1 } }""" + suffix

    // Enumeration list
    OAuthConsumerR.modify(_.privileges addToSet ConsumerPrivilege.awardBadges).toString() must_== """db.oauthconsumers.update({ }, { "$addToSet" : { "privileges" : "Award badges" } }""" + suffix

    // BsonRecordField and BsonRecordListField with nested Enumeration
    val claims = List(VenueClaimBson(1L, ClaimStatus.approved, Option(SourceBson("", "")), d1))
    VenueR.where(_.legacyid eqs 1).modify(_.claims setTo claims).toString() must_== query + """{ "$set" : { "claims" : [{ "date" : { "$date" : 1272672000000 }, "source" : { "url" : "", "name" : "" }, "status" : "Approved", "uid" : { "$numberLong" : "1" } }] } }""" + suffix

    //query + """{ "$set" : { "claims" : [ { "status" : "Approved", "uid" : 1, "source" : { "name" : "", "url" : "" }, "date" : { "$date" : "2010-05-01T00:00:00.000Z" }}]}}""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.lastClaim setTo claims.head).toString() must_== query + """{ "$set" : { "lastClaim" : { "date" : { "$date" : 1272672000000 }, "source" : { "url" : "", "name" : "" }, "status" : "Approved", "uid" : { "$numberLong" : "1" } } } }""" + suffix
    //"""{ "$set" : { "lastClaim" : { "status" : "Approved", "uid" : 1, "source" : { "name" : "", "url" : "" }, "date" : { "$date" : "2010-05-01T00:00:00.000Z" }}}}""".stripMargin + suffix

    // Map
    val m = Map("foo" -> 1L)
    val query3 = """db.tips.update({ "legid" : { "$numberLong" : "1" } }, """
    TipR.where(_.legacyid eqs 1).modify(_.counts setTo m).toString() must_== query3 + """{ "$set" : { "counts" : { "foo" : { "$numberLong" : "1" } } } }""" + suffix
    TipR.where(_.legacyid eqs 1).modify(_.counts at "foo" setTo 3).toString() must_== query3 + """{ "$set" : { "counts.foo" : { "$numberLong" : "3" } } }""" + suffix
    TipR.where(_.legacyid eqs 1).modify(_.counts at "foo" inc 5).toString() must_== query3 + """{ "$inc" : { "counts.foo" : 5 } }""" + suffix
    TipR.where(_.legacyid eqs 1).modify(_.counts at "foo" unset).toString() must_== query3 + """{ "$unset" : { "counts.foo" : 1 } }""" + suffix
    TipR.where(_.legacyid eqs 1).modify(_.counts setTo Map("foo" -> 3L, "bar" -> 5L)).toString() must_== query3 + """{ "$set" : { "counts" : { "bar" : { "$numberLong" : "5" }, "foo" : { "$numberLong" : "3" } } } }""" + suffix

    // Multiple updates
    VenueR.where(_.legacyid eqs 1).modify(_.venuename setTo "fshq").and(_.mayor_count setTo 3).toString() must_== query + """{ "$set" : { "mayor_count" : { "$numberLong" : "3" }, "venuename" : "fshq" } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.venuename setTo "fshq").and(_.mayor_count inc 1).toString() must_== query + """{ "$set" : { "venuename" : "fshq" }, "$inc" : { "mayor_count" : 1 } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.venuename setTo "fshq").and(_.mayor_count setTo 3).and(_.mayor_count inc 1).toString() must_== query + """{ "$set" : { "mayor_count" : { "$numberLong" : "3" }, "venuename" : "fshq" }, "$inc" : { "mayor_count" : 1 } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.popularity addToSet 3).and(_.tags addToSet List("a", "b")).toString() must_== query + """{ "$addToSet" : { "tags" : { "$each" : ["a", "b"] }, "popularity" : { "$numberLong" : "3" } } }""" + suffix

    // Noop query
    VenueR.where(_.legacyid eqs 1).noop().toString() must_== query + "{ }" + suffix
    VenueR.where(_.legacyid eqs 1).noop().modify(_.venuename setTo "fshq").toString() must_== query + """{ "$set" : { "venuename" : "fshq" } }""" + suffix
    VenueR.where(_.legacyid eqs 1).noop().and(_.venuename setTo "fshq").toString() must_== query + """{ "$set" : { "venuename" : "fshq" } }""" + suffix

    // $bit
    VenueR.where(_.legacyid eqs 1).modify(_.mayor_count bitAnd 3).toString() must_== query + """{ "$bit" : { "mayor_count" : { "and" : 3 } } }""" + suffix
    VenueR.where(_.legacyid eqs 1).modify(_.mayor_count bitOr 3).toString() must_== query + """{ "$bit" : { "mayor_count" : { "or" : 3 } } }""" + suffix

    // $rename
    VenueR.where(_.legacyid eqs 1).modify(_.venuename rename "vn").toString() must_== query + """{ "$rename" : { "venuename" : "vn" } }""" + suffix

    // $setOnInsert
    VenueR.where(_.legacyid eqs 1).modify(_.venuename setOnInsertTo "fshq").toString() must_== query + """{ "$setOnInsert" : { "venuename" : "fshq" } }""" + suffix

    // pullWhere
    /*
    object tags extends MongoListField[Venue, String](this)
    object popularity extends MongoListField[Venue, Long](this)
    object categories extends MongoListField[Venue, ObjectId](this)
    object claims extends BsonRecordListField(this, VenueClaimBson)
    */
    VenueR.where(_.legacyid eqs 1)
      .modify(_.tags pullWhere (_ startsWith "prefix"))
      .toString() must_== query + """{ "$pull" : { "tags" : { "$regex" : "^\\Qprefix\\E", "$options" : "" } } }""" + suffix
    VenueR.where(_.legacyid eqs 1)
      .modify(_.popularity pullWhere (_ gt 2))
      .toString() must_== query + """{ "$pull" : { "popularity" : { "$gt" : { "$numberLong" : "2" } } } }""" + suffix
    VenueR.where(_.legacyid eqs 1)
      .modify(_.popularity pullWhere (_ gt 2, _ lt 5))
      .toString() must_== query + """{ "$pull" : { "popularity" : { "$gt" : { "$numberLong" : "2" }, "$lt" : { "$numberLong" : "5" } } } }""" + suffix
    VenueR.where(_.legacyid eqs 1)
      .modify(_.claims pullObjectWhere (_.status eqs ClaimStatus.approved,
        _.uid eqs 2097))
      .toString() must_== query + """{ "$pull" : { "claims" : { "uid" : { "$numberLong" : "2097" }, "status" : "Approved" } } }""" + suffix
  }

  @Test
  def testProduceACorrectSignatureString {
    val d1 = LocalDateTime.of(2010, 5, 1, 0, 0, 0, 0)
    val d2 = LocalDateTime.of(2010, 5, 2, 0, 0, 0, 0)
    val oid = tag[Venue](new ObjectId)

    // basic ops
    VenueR.where(_.mayor eqs 1).signature() must_== """db.venues.find({ "mayor" : 0 })"""
    VenueR.where(_.venuename eqs "Starbucks").signature() must_== """db.venues.find({ "venuename" : 0 })"""
    VenueR.where(_.closed eqs true).signature() must_== """db.venues.find({ "closed" : 0 })"""
    VenueR.where(_.id eqs oid).signature() must_== """db.venues.find({ "_id" : 0 })"""
    VenueClaimR.where(_.status eqs ClaimStatus.approved).signature() must_== """db.venueclaims.find({ "status" : 0 })"""
    VenueR.where(_.mayor_count gte 5).signature() must_== """db.venues.find({ "mayor_count" : { "$gte" : 0 } })"""
    VenueClaimR.where(_.status neqs ClaimStatus.approved).signature() must_== """db.venueclaims.find({ "status" : { "$ne" : 0 } })"""
    VenueR.where(_.legacyid in List(123L, 456L)).signature() must_== """db.venues.find({ "legId" : { "$in" : 0 } })"""
    VenueR.where(_.id exists true).signature() must_== """db.venues.find({ "_id" : { "$exists" : 0 } })"""
    VenueR.where(_.venuename startsWith "Starbucks").signature() must_== """db.venues.find({ "venuename" : { "$regex" : 0, "$options" : 0 } })"""

    // list
    VenueR.where(_.tags all List("db", "ka")).signature() must_== """db.venues.find({ "tags" : { "$all" : 0 } })"""
    VenueR.where(_.tags in List("db", "ka")).signature() must_== """db.venues.find({ "tags" : { "$in" : 0 } })"""
    VenueR.where(_.tags size 3).signature() must_== """db.venues.find({ "tags" : { "$size" : 0 } })"""
    VenueR.where(_.tags contains "karaoke").signature() must_== """db.venues.find({ "tags" : 0 })"""
    VenueR.where(_.popularity contains 3).signature() must_== """db.venues.find({ "popularity" : 0 })"""
    VenueR.where(_.popularity at 0 eqs 3).signature() must_== """db.venues.find({ "popularity.0" : 0 })"""
    VenueR.where(_.categories at 0 eqs oid).signature() must_== """db.venues.find({ "categories.0" : 0 })"""
    VenueR.where(_.tags at 0 startsWith "kara").signature() must_== """db.venues.find({ "tags.0" : { "$regex" : 0, "$options" : 0 } })"""
    VenueR.where(_.tags idx 0 startsWith "kara").signature() must_== """db.venues.find({ "tags.0" : { "$regex" : 0, "$options" : 0 } })"""

    // map
    TipR.where(_.counts at "foo" eqs 3).signature() must_== """db.tips.find({ "counts.foo" : 0 })"""

    // near
    /*VenueR.where(_.geolatlng near (39.0, -74.0, Degrees(0.2)))    .signature() must_== """db.venues.find({ "latlng" : { "$near" : 0 }})"""
    VenueR.where(_.geolatlng withinCircle(1.0, 2.0, Degrees(0.3))).signature() must_== """db.venues.find({ "latlng" : { "$within" : { "$center" : 0 }}})"""
    VenueR.where(_.geolatlng withinBox(1.0, 2.0, 3.0, 4.0))       .signature() must_== """db.venues.find({ "latlng" : { "$within" : { "$box" : 0 }}})"""
    VenueR.where(_.geolatlng eqs (45.0, 50.0)).signature() must_== """db.venues.find({ "latlng" : 0 })"""
    VenueR.where(_.geolatlng nearSphere (39.0, -74.0, Radians(1.0)))    .signature() must_== """db.venues.find({ "latlng" : { "$nearSphere" : 0, "$maxDistance" : 0 }})"""
*/
    // id, date range
    VenueR.where(_.id before d2).signature() must_== """db.venues.find({ "_id" : { "$lt" : 0 } })"""
    VenueR.where(_.last_updated before d2).signature() must_== """db.venues.find({ "last_updated" : { "$lt" : 0 } })"""

    // Case class list field
    /*
    Comment.where(_.comments.unsafeField[Int]("z") contains 123)          .signature() must_== """db.comments.find({ "comments.z" : 0 })"""
    Comment.where(_.comments.unsafeField[String]("comment") contains "hi").signature() must_== """db.comments.find({ "comments.comment" : 0 })"""
*/

    // Enumeration list
    OAuthConsumerR.where(_.privileges contains ConsumerPrivilege.awardBadges).signature() must_== """db.oauthconsumers.find({ "privileges" : 0 })"""
    OAuthConsumerR.where(_.privileges at 0 eqs ConsumerPrivilege.awardBadges).signature() must_== """db.oauthconsumers.find({ "privileges.0" : 0 })"""

    // Field type
    VenueR.where(_.legacyid hastype MongoType.String).signature() must_== """db.venues.find({ "legId" : { "$type" : 0 } })"""

    // Modulus
    VenueR.where(_.legacyid mod (5, 1)).signature() must_== """db.venues.find({ "legId" : { "$mod" : 0 } })"""

    // compound queries
    VenueR.where(_.mayor eqs 1).and(_.tags contains "karaoke").signature() must_== """db.venues.find({ "mayor" : 0, "tags" : 0 })"""
    VenueR.where(_.mayor eqs 1).and(_.mayor_count gt 3).and(_.mayor_count lt 5).signature() must_== """db.venues.find({ "mayor" : 0, "mayor_count" : { "$lt" : 0, "$gt" : 0 } })"""

    // queries with no clauses
    ccMetaToQueryBuilder(VenueR).signature() must_== "db.venues.find({ })"
    VenueR.orderDesc(_.id).signature() must_== """db.venues.find({ }).sort({ "_id" : -1 })"""

    // ordered queries
    VenueR.where(_.mayor eqs 1).orderAsc(_.legacyid).signature() must_== """db.venues.find({ "mayor" : 0 }).sort({ "legId" : 1 })"""
    VenueR.where(_.mayor eqs 1).orderDesc(_.legacyid).andAsc(_.userId).signature() must_== """db.venues.find({ "mayor" : 0 }).sort({ "legId" : -1, "userId" : 1 })"""

    // select queries
    VenueR.where(_.mayor eqs 1).select(_.legacyid).signature() must_== """db.venues.find({ "mayor" : 0 })"""

    // Scan should be the same as and/where
    VenueR.where(_.mayor eqs 1).scan(_.tags contains "karaoke").signature() must_== """db.venues.find({ "mayor" : 0, "tags" : 0 })"""

    // or queries
    VenueR.where(_.mayor eqs 1).or(_.where(_.id eqs oid)).signature() must_== """db.venues.find({ "mayor" : 0, "$or" : [{ "_id" : 0 }] })"""
  }

  @Test
  def testFindAndModifyQueryShouldProduceACorrectJSONQueryString {
    VenueR.where(_.legacyid eqs 1).findAndModify(_.venuename setTo "fshq").toString().must_==(
      """db.venues.findAndModify({ query: { "legId" : { "$numberLong" : "1" } }, update: { "$set" : { "venuename" : "fshq" } }, new: false, upsert: false })""")
    VenueR.where(_.legacyid eqs 1).orderAsc(_.popularity).findAndModify(_.venuename setTo "fshq").toString().must_==(
      """db.venues.findAndModify({ query: { "legId" : { "$numberLong" : "1" } }, sort: { "popularity" : 1 }, update: { "$set" : { "venuename" : "fshq" } }, new: false, upsert: false })""")
    VenueR.where(_.legacyid eqs 1).select(_.mayor, _.closed).findAndModify(_.venuename setTo "fshq").toString().must_==(
      """db.venues.findAndModify({ query: { "legId" : { "$numberLong" : "1" } }, update: { "$set" : { "venuename" : "fshq" } }, new: false, fields: { "mayor" : 1, "closed" : 1 }, upsert: false })""")
  }

  @Test
  def testOrQueryShouldProduceACorrectJSONQueryString {
    // Simple $or
    VenueR.or(
      _.where(_.legacyid eqs 1),
      _.where(_.mayor eqs 2))
      .toString() must_== """db.venues.find({ "$or" : [{ "legId" : { "$numberLong" : "1" } }, { "mayor" : { "$numberLong" : "2" } }] })"""

    // Compound $or
    VenueR.where(_.tags size 0)
      .or(
        _.where(_.legacyid eqs 1),
        _.where(_.mayor eqs 2))
      .toString() must_== """db.venues.find({ "tags" : { "$size" : 0 }, "$or" : [{ "legId" : { "$numberLong" : "1" } }, { "mayor" : { "$numberLong" : "2" } }] })"""

    // $or with additional "and" clauses
    VenueR.where(_.tags size 0)
      .or(
        _.where(_.legacyid eqs 1).and(_.closed eqs true),
        _.where(_.mayor eqs 2))
      .toString() must_== """db.venues.find({ "tags" : { "$size" : 0 }, "$or" : [{ "legId" : { "$numberLong" : "1" }, "closed" : true }, { "mayor" : { "$numberLong" : "2" } }] })"""

    // Nested $or
    VenueR.or(
      _.where(_.legacyid eqs 1)
        .or(
          _.where(_.closed eqs true),
          _.where(_.closed exists false)),
      _.where(_.mayor eqs 2))
      .toString() must_== """db.venues.find({ "$or" : [{ "legId" : { "$numberLong" : "1" }, "$or" : [{ "closed" : true }, { "closed" : { "$exists" : false } }] }, { "mayor" : { "$numberLong" : "2" } }] })"""

    // $or with modify
    VenueR.or(
      _.where(_.legacyid eqs 1),
      _.where(_.mayor eqs 2))
      .modify(_.userId setTo 1).toString() must_== """db.venues.update({ "$or" : [{ "legId" : { "$numberLong" : "1" } }, { "mayor" : { "$numberLong" : "2" } }] }, { "$set" : { "userId" : { "$numberLong" : "1" } } }, false, false)"""

    // $or with optional where clause
    VenueR.or(
      _.where(_.legacyid eqs 1),
      _.whereOpt(None)(_.mayor eqs _))
      .modify(_.userId setTo 1).toString() must_== """db.venues.update({ "$or" : [{ "legId" : { "$numberLong" : "1" } }] }, { "$set" : { "userId" : { "$numberLong" : "1" } } }, false, false)"""

    VenueR.or(
      _.where(_.legacyid eqs 1),
      _.whereOpt(Some(2))(_.mayor eqs _))
      .modify(_.userId setTo 1).toString() must_== """db.venues.update({ "$or" : [{ "legId" : { "$numberLong" : "1" } }, { "mayor" : { "$numberLong" : "2" } }] }, { "$set" : { "userId" : { "$numberLong" : "1" } } }, false, false)"""

    // OrQuery syntax
    val q1 = VenueR.where(_.legacyid eqs 1)
    val q2 = VenueR.where(_.legacyid eqs 2)
    OrQuery(q1, q2).toString() must_==
      """db.venues.find({ "$or" : [{ "legId" : { "$numberLong" : "1" } }, { "legId" : { "$numberLong" : "2" } }] })"""
    OrQuery(q1, q2).and(_.mayor eqs 0).toString() must_==
      """db.venues.find({ "mayor" : { "$numberLong" : "0" }, "$or" : [{ "legId" : { "$numberLong" : "1" } }, { "legId" : { "$numberLong" : "2" } }] })"""
    OrQuery(q1, q2.or(_.where(_.closed eqs true), _.where(_.closed exists false))).toString() must_==
      """db.venues.find({ "$or" : [{ "legId" : { "$numberLong" : "1" } }, { "legId" : { "$numberLong" : "2" }, "$or" : [{ "closed" : true }, { "closed" : { "$exists" : false } }] }] })"""
  }

  @Test
  def testHints {
    VenueR.where(_.legacyid eqs 1).hint(VenueR.idIdx).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }).hint({ "_id" : 1 })"""
    VenueR.where(_.legacyid eqs 1).hint(VenueR.legIdx).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }).hint({ "legId" : -1 })"""
    VenueR.where(_.legacyid eqs 1).hint(VenueR.legIdIdx).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }).hint({ "legId" : 1, "_id" : -1 })"""
    //no support for geo indexes yet in test
    //VenueR.where(_.legacyid eqs 1).hint(VenueR.geoIdx).toString()       must_== """db.venues.find({ "legId" : 1 }).hint({ "latlng" : "2d" })"""
    //VenueR.where(_.legacyid eqs 1).hint(VenueR.geoCustomIdx).toString() must_== """db.venues.find({ "legId" : 1 }).hint({ "latlng" : "custom", "tags" : 1 })"""

    //optional hints
    VenueR.where(_.legacyid eqs 1).hintOpt(Some(VenueR.idIdx)).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }).hint({ "_id" : 1 })"""
    VenueR.where(_.legacyid eqs 1).hintOpt(Some(VenueR.legIdx)).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } }).hint({ "legId" : -1 })"""
    VenueR.where(_.legacyid eqs 1).hintOpt(None).toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" } })"""
  }

  @Test
  def testDollarSelector {

    VenueR.where(_.legacyid eqs 1)
      .and(_.claims.subfield(_.uid) contains 2)
      .modify(_.claims.$.subfield(_.status) setTo ClaimStatus.approved)
      .toString() must_== """db.venues.update({ "legId" : { "$numberLong" : "1" }, "claims.uid" : { "$numberLong" : "2" } }, { "$set" : { "claims.$.status" : "Approved" } }, false, false)"""

    VenueR.where(_.legacyid eqs 1)
      .and(_.tags contains "sometag")
      .modify(_.tags.$ setTo "othertag")
      .toString() must_== """db.venues.update({ "legId" : { "$numberLong" : "1" }, "tags" : "sometag" }, { "$set" : { "tags.$" : "othertag" } }, false, false)"""

    VenueR.where(_.legacyid eqs 1)
      .and(_.tags contains "sometag")
      .select(_.tags.$$)
      .toString() must_== """db.venues.find({ "legId" : { "$numberLong" : "1" }, "tags" : "sometag" }, { "tags.$" : 1 })"""
  }
  /*
      @Test
      def testWhereOpt {
        val someId = Some(1L)
        val noId: Option[Long] = None
        val someList = Some(List(1L, 2L))
        val noList: Option[List[Long]] = None

        // whereOpt
        VenueR.whereOpt(someId)(_.legacyid eqs _).toString() must_== """db.venues.find({ "legId" : 1 })"""
        VenueR.whereOpt(noId)(_.legacyid eqs _).toString() must_== """db.venues.find({ })"""
        VenueR.whereOpt(someId)(_.legacyid eqs _).and(_.mayor eqs 2).toString() must_== """db.venues.find({ "legId" : 1, "mayor" : 2})"""
        VenueR.whereOpt(noId)(_.legacyid eqs _).and(_.mayor eqs 2).toString() must_== """db.venues.find({ "mayor" : 2})"""

        // whereOpt: lists
        VenueR.whereOpt(someList)(_.legacyid in _).toString() must_== """db.venues.find({ "legId" : { "$in" : [ 1, 2 ] }})"""
        VenueR.whereOpt(noList)(_.legacyid in _).toString() must_== """db.venues.find({ })"""

        // whereOpt: enum
        val someEnum = Some(VenueStatus.open)
        val noEnum: Option[VenueStatus.type#Value] = None
        VenueR.whereOpt(someEnum)(_.status eqs _).toString() must_== """db.venues.find({ "status" : "Open" })"""
        VenueR.whereOpt(noEnum)(_.status eqs _).toString() must_== """db.venues.find({ })"""

        // whereOpt: date
        val someDate = Some(new DateTime(2010, 5, 1, 0, 0, 0, 0, DateTimeZone.UTC))
        val noDate: Option[DateTime] = None
        VenueR.whereOpt(someDate)(_.last_updated after _).toString() must_== """db.venues.find({ "last_updated" : { "$gt" : { "$date" : "2010-05-01T00:00:00.000Z" }}})"""
        VenueR.whereOpt(noDate)(_.last_updated after _).toString() must_== """db.venues.find({ })"""

        // andOpt
        VenueR.where(_.mayor eqs 2).andOpt(someId)(_.legacyid eqs _).toString() must_== """db.venues.find({ "mayor" : 2, "legid" : 1 })"""
        VenueR.where(_.mayor eqs 2).andOpt(noId)(_.legacyid eqs _).toString() must_== """db.venues.find({ "mayor" : 2})"""

        // scanOpt
        VenueR.scanOpt(someId)(_.legacyid eqs _).toString() must_== """db.venues.find({ "legId" : 1 })"""
        VenueR.scanOpt(noId)(_.legacyid eqs _).toString() must_== """db.venues.find({ })"""
        VenueR.scanOpt(someId)(_.legacyid eqs _).and(_.mayor eqs 2).toString() must_== """db.venues.find({ "legId" : 1, "mayor" : 2})"""
        VenueR.scanOpt(noId)(_.legacyid eqs _).and(_.mayor eqs 2).toString() must_== """db.venues.find({ "mayor" : 2})"""

        // iscanOpt
        VenueR.iscanOpt(someId)(_.legacyid eqs _).toString() must_== """db.venues.find({ "legId" : 1 })"""
        VenueR.iscanOpt(noId)(_.legacyid eqs _).toString() must_== """db.venues.find({ })"""
        VenueR.iscanOpt(someId)(_.legacyid eqs _).and(_.mayor eqs 2).toString() must_== """db.venues.find({ "legId" : 1, "mayor" : 2})"""
        VenueR.iscanOpt(noId)(_.legacyid eqs _).and(_.mayor eqs 2).toString() must_== """db.venues.find({ "mayor" : 2})"""

        // modify
        val q = VenueR.where(_.legacyid eqs 1)
        val prefix = """db.venues.update({ "legid" : 1 }, """
        val suffix = ", false, false)"

        q.modifyOpt(someId)(_.legacyid setTo _).toString() must_== prefix + """{ "$set" : { "legid" : 1 }}""" + suffix
        q.modifyOpt(noId)(_.legacyid setTo _).toString() must_== prefix + """{ }""" + suffix
        q.modifyOpt(someEnum)(_.status setTo _).toString() must_== prefix + """{ "$set" : { "status" : "Open" }}""" + suffix
        q.modifyOpt(noEnum)(_.status setTo _).toString() must_== prefix + """{ }""" + suffix
      }

      @Test
      def testShardKey {
        LikeR.where(_.checkin eqs 123).toString() must_== """db.likes.find({ "checkin" : 123 })"""
        LikeR.where(_.userid eqs 123).toString() must_== """db.likes.find({ "userid" : 123 })"""
        LikeR.where(_.userid eqs 123).allShards.toString() must_== """db.likes.find({ "userid" : 123 })"""
        LikeR.where(_.userid eqs 123).allShards.noop().toString() must_== """db.likes.update({ "userid" : 123 }, { }, false, false)"""
        LikeR.withShardKey(_.userid eqs 123).toString() must_== """db.likes.find({ "userid" : 123 })"""
        LikeR.withShardKey(_.userid in List(123L, 456L)).toString() must_== """db.likes.find({ "userid" : { "$in" : [ 123, 456]}})"""
        LikeR.withShardKey(_.userid eqs 123).and(_.checkin eqs 1).toString() must_== """db.likes.find({ "userid" : 123, "checkin" : 1 })"""
        LikeR.where(_.checkin eqs 1).withShardKey(_.userid eqs 123).toString() must_== """db.likes.find({ "checkin" : 1, "userid" : 123 })"""
      }

      @Test
      def testCommonSuperclassForPhantomTypes {
        def maybeLimit(legid: Long, limitOpt: Option[Int]) = {
          limitOpt match {
            case Some(limit) => VenueR.where(_.legacyid eqs legid).limit(limit)
            case None => VenueR.where(_.legacyid eqs legid)
          }
        }

        maybeLimit(1, None).toString() must_== """db.venues.find({ "legId" : 1 })"""
        maybeLimit(1, Some(5)).toString() must_== """db.venues.find({ "legId" : 1 }).limit(5)"""
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

  @Test
  def uuidTest(): Unit = {
    val u = UUID.randomUUID()
    //it is ok it just compiles
    UuidCcR.where(_.id eqs u)
    UuidCcR.where(_.dt gte Instant.now())
  }

  @Test
  def testNullEq(): Unit = {
    val q1 = UuidCcR.where(_.dt eqsNull).asDBObject
    val q2 = UuidCcR.where(_.dt neqsNull).asDBObject
    q1.toString must_== """{ "i" : null }"""
    q2.toString must_== """{ "i" : { "$ne" : null } }"""
  }
}
