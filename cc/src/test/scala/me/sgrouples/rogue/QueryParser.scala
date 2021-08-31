package me.sgrouples.rogue
import com.mongodb.{BasicDBObject, BasicDBObjectBuilder}
import io.fsq.rogue.MongoHelpers.MongoBuilder
import io.fsq.rogue.{
  FindAndModifyQuery,
  ModifyQuery,
  MongoHelpers,
  PartialIndexScan,
  Query,
  RegexQueryClause
}
import me.sgrouples.rogue.QueryParser.pq
import munit.FunSuite
import org.bson.{BsonBoolean, BsonInt64, BsonRegularExpression, Document}
import org.mongodb.scala.bson.{BsonDocument, BsonString, BsonValue}

import java.util.regex.Pattern
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
//TODO - hints support
case class ParsedQuery(
    collection: String,
    command: String,
    args: Seq[BsonDocument],
    limit: Option[Int] = None
) {}
object QueryParser {
  //parses string to mongo query
  //of shape db.collection.op()[.hint({})]
  //pq aka parseQuery
  def pq(in: String): ParsedQuery = {
    var idx = 0
    if (!in.startsWith("db.")) {
      throw new RuntimeException("Not a mongo query")
    } else {
      idx = 3
      while (in(idx) != '.') {
        idx = idx + 1
      }
      val collection = in.substring(3, idx)
      idx = idx + 1
      val opStart = idx
      while (in(idx) != '(') {
        idx = idx + 1
      }
      val command = in.substring(opStart, idx)
      var prev = in(idx)
      idx = idx + 1
      var prevStart = idx
      //now expression
      var level = 0
      var inParens = false
      val args = Seq.newBuilder[BsonDocument]
      var limitOpt: Option[Int] = None
      while (idx < in.length) {
        in(idx) match {
          case '{' if prev != '\\' && !inParens =>
            level = level + 1
          case '}' if prev != '\\' && !inParens =>
            level = level - 1
            if (level == 0) {
              try {
                args += Document
                  .parse(in.substring(prevStart, idx + 1))
                  .toBsonDocument
                prevStart = idx + 1
              } catch {
                case e: org.bson.json.JsonParseException =>
                  println(s"Can't parse ${in.substring(prevStart, idx + 1)}")
                  throw e
              }
            }
          case '"' if prev != '\\' => inParens = !inParens
          case ' ' | ',' if level == 0 =>
            prevStart = prevStart + 1
          case '.' if level == 0 =>
            if (in.substring(idx).startsWith(".hint")) {
              prevStart = idx + 6
              idx = idx + 6
            } else if (in.substring(idx).startsWith(".sort")) {
              prevStart = idx + 6
              idx = idx + 6
            } else if (in.substring(idx).startsWith(".limit")) {
              idx = idx + 7
              prevStart = idx
              while (in(idx).isDigit) {
                idx = idx + 1
              }
              limitOpt = Some(in.substring(prevStart, idx).toInt)
              prevStart = idx
            }
          case _ =>
        }
        prev = in(idx)
        idx = idx + 1
      }
      ParsedQuery(collection, command, args.result(), limitOpt)
    }
  }

  def regexReplace(in: BsonDocument): BsonValue = {
    if (in.containsKey("$regex") && in.containsKey("$options")) {
      val r = in.get("$regex").asString().getValue
      val o = in.get("$options").asString().getValue
      val re = new BsonRegularExpression(r, o)
      in.remove("$regex")
      in.remove("$options")
      if (in.keySet().isEmpty) {
        re
      } else {
        in.put("$regex", BsonString(re.getPattern))
        in.put("$options", BsonString(re.getOptions))
        in
      }
    } else {
      val it = in.entrySet().iterator()
      while (it.hasNext) {
        val e = it.next()
        val k = e.getKey
        val v = e.getValue
        if (v.isDocument) {
          val replacement = regexReplace(v.asDocument())
          if (replacement.isRegularExpression) {
            if (v.asDocument().isEmpty) {
              in.put(k, replacement)
            }
          }
        }
      }
      in
    }
  }

  implicit class QueryWrapper[M, R, +State](val query: Query[M, R, State])
      extends AnyVal {
    def q: ParsedQuery = {
      val args = new ListBuffer[BsonDocument]
      args.addOne(
        regexReplace(
          query.asDBObject.asInstanceOf[BasicDBObject].toBsonDocument()
        ).asDocument()
      )
      query.select.foreach(s =>
        args.addOne(
          regexReplace(MongoBuilder.buildSelect(s).toBsonDocument())
            .asDocument()
        )
      )
      ParsedQuery(query.collectionName, "find", args.result(), query.lim)
    }
  }
  implicit class ModifyQueryWrapper[M, +State](val query: ModifyQuery[M, State])
      extends AnyVal {
    def q: ParsedQuery = {
      val (q, m) = query.asDBObject
      ParsedQuery(
        query.query.collectionName,
        "update",
        List(
          regexReplace(q.asInstanceOf[BasicDBObject].toBsonDocument())
            .asDocument(),
          regexReplace(m.toBsonDocument()).asDocument()
        )
      )
    }
  }

  implicit class FindAndModifyQueryWrapper[M, State](
      val query: FindAndModifyQuery[M, State]
  ) extends AnyVal {
    def q: ParsedQuery = {
      val q = query.query.asDBObject
      val m = MongoBuilder.buildModify(query.mod)
      val res = {
        val tmp = new BasicDBObject(
          "query",
          regexReplace(q.asInstanceOf[BasicDBObject].toBsonDocument())
            .asDocument()
        )
        query.query.order.foreach(o =>
          tmp.append("sort", MongoBuilder.buildOrder(o))
        )
        tmp
          .append("update", regexReplace(m.toBsonDocument()))
          .append("new", false)
        query.query.select.foreach(s =>
          tmp.append("fields", MongoBuilder.buildSelect(s))
        )
        tmp
      }.toBsonDocument()
        .append("upsert", BsonBoolean.FALSE)
      ParsedQuery(query.query.collectionName, "findAndModify", List(res))
    }
  }
}

class QueryParserTest extends FunSuite {
  test("base parsing") {
    val parsed = QueryParser.pq(
      """db.venues.find({"mayor_count": {"$gte": {"$numberLong": "3"}, "$lte": {"$numberLong": "5"}}})"""
    )
    val expected = ParsedQuery(
      "venues",
      "find",
      List(
        new Document(
          "mayor_count",
          new Document("$gte", new BsonInt64(3L))
            .append("$lte", new BsonInt64(5L))
        ).toBsonDocument
      )
    )
    val x = parsed.args.head
    val y = expected.args.head

    assertEquals(parsed, expected)
  }
  test("multi args") {
    val parsed = QueryParser.pq(
      """db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "mayor": 1, "mayor_count": 1, "_id": 0})"""
    )
    val expected = ParsedQuery(
      "venues",
      "find",
      List(
        new Document("mayor", new BsonInt64(1L)).toBsonDocument,
        new Document("legId", 1)
          .append("userId", 1)
          .append("mayor", 1)
          .append("mayor_count", 1)
          .append("_id", 0)
          .toBsonDocument
      )
    )
    assertEquals(parsed, expected)
  }

  test("Regex") {
    val c = new RegexQueryClause(
      "r",
      PartialIndexScan,
      Pattern.compile("^\\QStarbucks\\E")
    )
    val b = new BasicDBObjectBuilder
    b.push("some key")
    c.extend(b, false)
    val doc = b.get().asInstanceOf[BasicDBObject].toBsonDocument()
    val before = doc.toString
    val replaced = QueryParser.regexReplace(doc)
    assertEquals(
      before,
      """{"some key": {"$regex": "^\\QStarbucks\\E", "$options": ""}}"""
    )
    assertEquals(
      replaced.toString,
      """{"some key": {"$regularExpression": {"pattern": "^\\QStarbucks\\E", "options": ""}}}"""
    )
  }
}
