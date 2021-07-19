package me.sgrouples.rogue
import com.mongodb.BasicDBObject
import io.fsq.rogue.{FindAndModifyQuery, ModifyQuery, Query}
import munit.FunSuite
import org.bson.{BsonInt64, Document}
import org.mongodb.scala.bson.BsonDocument
//TODO - hints support
case class ParsedQuery(
                        collection: String,
                        command: String,
                        args: Seq[BsonDocument]) {
}
object QueryParser {
  //parses string to mongo query
  //of shape db.collection.op()
  //pq aka parseQuery
  def pq(in:String): ParsedQuery = {
    var idx = 0
    if(!in.startsWith("db.")) {
      throw new RuntimeException("Not a mongo query")
    } else {
      idx = 3
      while(in(idx)!='.') {
        idx = idx + 1
      }
      val collection = in.substring(3,idx)
      idx = idx+1
      val opStart = idx
      while(in(idx)!='(') {
        idx = idx + 1
      }
      val command = in.substring(opStart,idx)
      var prev = in(idx)
      idx = idx+1
      var prevStart = idx
      //now expression
      var level = 0
      var inParens = false
      val args = Seq.newBuilder[BsonDocument]
      while(idx < in.length) {
        in(idx) match {
          case '{' if prev !='\\' && !inParens =>
            level = level +1
          case '}' if prev !='\\' && !inParens =>
            level = level -1
            if(level == 0) {
              args += Document.parse(in.substring(prevStart, idx+1)).toBsonDocument
              prevStart = idx + 1
            }
          case '"' if prev != '\\' => inParens = !inParens
          case ' ' | ',' if level == 0 =>
            prevStart = prevStart + 1
          case _ =>
        }
        prev = in(idx)
        idx = idx + 1
        }
        ParsedQuery(collection, command, args.result())
      }
    }
  implicit class QueryWrapper[M, R, +State](val query:Query[M, R, State]) extends AnyVal {
    def q:ParsedQuery = {
      val ob = query.asDBObject.asInstanceOf[BasicDBObject].toBsonDocument()
      ParsedQuery(query.collectionName, "find", List(ob))
    }
  }
  implicit class ModifyQueryWrapper[M, +State](val query:ModifyQuery[M, State]) extends AnyVal {
    def q:ParsedQuery = {
      ???
    }
  }
  implicit class FindAndModifyQueryWrapper[M, State](val query:FindAndModifyQuery[M, State]) extends AnyVal {
    def q:ParsedQuery = {
      ???
    }
  }
}


class QueryParserTest extends FunSuite {
  test("base parsing") {
    val parsed = QueryParser.pq("""db.venues.find({"mayor_count": {"$gte": {"$numberLong": "3"}, "$lte": {"$numberLong": "5"}}})""")
    val expected = ParsedQuery("venues","find", List(new Document("mayor_count",
      new Document("$gte", new BsonInt64(3L))
        .append("$lte", new BsonInt64(5L))
    ).toBsonDocument))
    val x = parsed.args.head
    val y = expected.args.head

    assertEquals(parsed, expected)
  }
  test("multi args") {
    val parsed = QueryParser.pq("""db.venues.find({"mayor": {"$numberLong": "1"}}, {"legId": 1, "userId": 1, "mayor": 1, "mayor_count": 1, "_id": 0})""")
    val expected = ParsedQuery("venues","find", List(
      new Document("mayor",new BsonInt64(1L)).toBsonDocument,
      new Document("legId", 1)
        .append("userId",1)
        .append("mayor", 1)
        .append("mayor_count", 1)
        .append("_id", 0).toBsonDocument
    ))
    assertEquals(parsed, expected)
  }
}