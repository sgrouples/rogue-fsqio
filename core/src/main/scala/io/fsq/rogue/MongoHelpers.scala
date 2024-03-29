// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue

import com.mongodb.{BasicDBObject, BasicDBObjectBuilder, DBObject}
import java.util.regex.Pattern

import io.fsq.rogue.index.MongoIndex
import org.bson.BsonDocument

object MongoHelpers extends Rogue {
  case class AndCondition(
      clauses: List[QueryClause[_]],
      orCondition: Option[OrCondition],
      searchCondition: Option[SearchCondition]
  ) {
    def isEmpty: Boolean =
      clauses.isEmpty && orCondition.isEmpty && searchCondition.isEmpty
  }

  case class OrCondition(conditions: List[AndCondition])

  case class SearchCondition(
      search: String,
      language: Option[String],
      caseSensitivity: Option[Boolean],
      diacriticSensitive: Option[Boolean]
  )

  sealed trait MongoOrderTerm {
    def extend(builder: BasicDBObjectBuilder): Unit
  }
  case class FieldOrderTerm(field: String, ascending: Boolean)
      extends MongoOrderTerm {
    def extend(builder: BasicDBObjectBuilder): Unit =
      builder.add(field, if (ascending) 1 else -1)
  }
  case class NaturalOrderTerm(ascending: Boolean) extends MongoOrderTerm {
    def extend(builder: BasicDBObjectBuilder): Unit =
      builder.add("$natural", if (ascending) 1 else -1)
  }
  case class ScoreOrderTerm(name: String) extends MongoOrderTerm {
    def extend(builder: BasicDBObjectBuilder): Unit =
      builder.push(name).add("$meta", "textScore").pop
  }

  sealed case class MongoOrder(terms: List[MongoOrderTerm])

  sealed case class MongoModify(clauses: List[ModifyClause])

  sealed case class MongoSelect[M, R](
      fields: IndexedSeq[SelectField[_, M]],
      transformer: IndexedSeq[Any] => R,
      isExists: Boolean,
      scoreName: Option[String]
  )

  object MongoBuilder {
    def buildCondition(
        cond: AndCondition,
        signature: Boolean = false
    ): BasicDBObject = {
      buildCondition(cond, BasicDBObjectBuilder.start, signature)
    }

    def buildCondition(
        cond: AndCondition,
        builder: BasicDBObjectBuilder,
        signature: Boolean
    ): BasicDBObject = {
      val (rawClauses, safeClauses) =
        cond.clauses.partition(_.isInstanceOf[RawQueryClause])

      // Normal clauses
      safeClauses
        .groupBy(_.fieldName)
        .toList
        .sortBy { case (fieldName, _) =>
          -cond.clauses.indexWhere(_.fieldName == fieldName)
        }
        .foreach {
          case (name, cs) => {
            // Equality clauses look like { a : 3 }
            // but all other clauses look like { a : { $op : 3 }}
            // and can be chained like { a : { $gt : 2, $lt: 6 }}.
            // So if there is any equality clause, apply it (only) to the builder;
            // otherwise, chain the clauses.
            cs.filter(_.isInstanceOf[EqClause[_, _]]).headOption match {
              case Some(eqClause) => eqClause.extend(builder, signature)
              case None => {
                builder.push(name)
                val (negative, positive) = cs.partition(_.negated)
                positive.foreach(_.extend(builder, signature))
                if (negative.nonEmpty) {
                  builder.push("$not")
                  negative.foreach(_.extend(builder, signature))
                  builder.pop
                }
                builder.pop
              }
            }
          }
        }

      // Raw clauses
      rawClauses.foreach(_.extend(builder, signature))

      // Optional $or clause (only one per "and" chain)
      cond.orCondition.foreach(or => {
        val subclauses = or.conditions
          .map(buildCondition(_, signature))
          .filterNot(_.keySet.isEmpty)
        builder.add("$or", QueryHelpers.list(subclauses))
      })

      // Optional $text clause
      cond.searchCondition.foreach(txt => {
        builder.push("$text").add("$search", txt.search)
        txt.language.foreach { lang => builder.add("$language", lang) }
        txt.caseSensitivity.foreach { cs => builder.add("$caseSensitive", cs) }
        txt.diacriticSensitive.foreach { ds =>
          builder.add("$diacriticSensitive", ds)
        }
        builder.pop
      })

      builder.get.asInstanceOf[BasicDBObject]
    }

    def buildOrder(o: MongoOrder): BasicDBObject = {
      val builder = BasicDBObjectBuilder.start
      o.terms.reverse.foreach(_.extend(builder))
      builder.get.asInstanceOf[BasicDBObject]
    }

    def buildModify(m: MongoModify): BasicDBObject = {
      val builder = BasicDBObjectBuilder.start
      m.clauses.groupBy(_.operator).foreach {
        case (op, cs) => {
          builder.push(op.toString)
          cs.foreach(_.extend(builder))
          builder.pop
        }
      }
      builder.get().asInstanceOf[BasicDBObject]
    }

    def buildSelect[M, R](select: MongoSelect[M, R]): BasicDBObject = {
      val builder = BasicDBObjectBuilder.start
      // If select.isExists is true, then a MongoSelect clause exists,
      // and we select just the _id field.
      if (select.isExists) {
        builder.add("_id", 1)
      } else {
        var hasId = false
        select.fields.foreach(f => {
          val fName = f.field.name
          if (fName == "_id") hasId = true
          f.slc match {
            case None            => builder.add(fName, 1)
            case Some((s, None)) => builder.push(fName).add("$slice", s).pop()
            case Some((s, Some(e))) =>
              builder
                .push(fName)
                .add("$slice", QueryHelpers.makeJavaList(List(s, e)))
                .pop()
          }
        })
        if (!hasId) {
          builder.add("_id", 0)
        }
      }

      // add score "field"
      select.scoreName.foreach(name =>
        builder.push(name).add("$meta", "textScore").pop
      )
      builder.get.asInstanceOf[BasicDBObject]
    }

    def buildHint[M](idx: MongoIndex[M]): BsonDocument = {
      idx.asBsonDocument
    }

    val OidPattern = Pattern.compile("""\{ "\$oid" : "([0-9a-f]{24})"\}""")
    def stringFromDBObject(dbo: DBObject): String = {
      // DBObject.toString renders ObjectIds like { $oid: "..."" }, but we want ObjectId("...")
      // because that's the format the Mongo REPL accepts.
      OidPattern.matcher(dbo.toString).replaceAll("""ObjectId("$1")""")
    }

    def buildQueryString[R, M](
        operation: String,
        collectionName: String,
        query: Query[M, R, _]
    ): String = {
      val sb = new StringBuilder("db.%s.%s(".format(collectionName, operation))
      sb.append(
        stringFromDBObject(buildCondition(query.condition, signature = false))
      )
      query.select.foreach(s => sb.append(", " + buildSelect(s).toString))
      sb.append(")")
      query.order.foreach(o =>
        sb.append(".sort(%s)" format buildOrder(o).toString)
      )
      query.lim.foreach(l => sb.append(".limit(%d)" format l))
      query.sk.foreach(s => sb.append(".skip(%d)" format s))
      query.maxScan.foreach(m =>
        sb.append("._addSpecial(\"$maxScan\", %d)" format m)
      )
      query.comment.foreach(c =>
        sb.append("._addSpecial(\"$comment\", \"%s\")" format c)
      )
      query.hint.foreach(h =>
        sb.append(".hint(%s)" format buildHint(h).toString)
      )
      sb.toString
    }

    def buildConditionString[R, M](
        operation: String,
        collectionName: String,
        query: Query[M, R, _]
    ): String = {
      val sb = new StringBuilder("db.%s.%s(".format(collectionName, operation))
      sb.append(buildCondition(query.condition, signature = false).toString)
      sb.append(")")
      sb.toString
    }

    def buildModifyString[R, M](
        collectionName: String,
        modify: ModifyQuery[M, _],
        upsert: Boolean = false,
        multi: Boolean = false
    ): String = {
      "db.%s.update(%s, %s, %s, %s)".format(
        collectionName,
        stringFromDBObject(
          buildCondition(modify.query.condition, signature = false)
        ),
        stringFromDBObject(buildModify(modify.mod)),
        upsert,
        multi
      )
    }

    def buildFindAndModifyString[R, M](
        collectionName: String,
        mod: FindAndModifyQuery[M, R],
        returnNew: Boolean,
        upsert: Boolean,
        remove: Boolean
    ): String = {
      val query = mod.query
      val sb = new StringBuilder(
        "db.%s.findAndModify({ query: %s".format(
          collectionName,
          stringFromDBObject(buildCondition(query.condition))
        )
      )
      query.order.foreach(o => sb.append(", sort: " + buildOrder(o).toString))
      if (remove) sb.append(", remove: true")
      sb.append(", update: " + stringFromDBObject(buildModify(mod.mod)))
      sb.append(", new: " + returnNew)
      query.select.foreach(s =>
        sb.append(", fields: " + buildSelect(s).toString)
      )
      sb.append(", upsert: " + upsert)
      sb.append(" })")
      sb.toString
    }

    def buildSignature[R, M](
        collectionName: String,
        query: Query[M, R, _]
    ): String = {
      val sb = new StringBuilder("db.%s.find(".format(collectionName))
      sb.append(buildCondition(query.condition, signature = true).toString)
      sb.append(")")
      query.order.foreach(o =>
        sb.append(".sort(%s)" format buildOrder(o).toString)
      )
      sb.toString
    }
  }
}
