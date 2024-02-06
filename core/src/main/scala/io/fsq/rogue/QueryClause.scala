// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue

import com.mongodb.BasicDBObjectBuilder
import java.util.regex.Pattern
import scala.Iterable

abstract class QueryClause[V](
    val fieldName: String,
    val actualIndexBehavior: MaybeIndexed,
    val conditions: (CondOps.Value, V)*
) {
  def extend(q: BasicDBObjectBuilder, signature: Boolean): Unit = {
    conditions foreach { case (op, v) =>
      q.add(op.toString, if (signature) 0 else v)
    }
  }
  var negated: Boolean = false
  var expectedIndexBehavior: MaybeIndexed = Index
}

abstract class IndexableQueryClause[V, Ind <: MaybeIndexed](
    fname: String,
    actualIB: Ind,
    conds: (CondOps.Value, V)*
) extends QueryClause[V](fname, actualIB, conds: _*)

trait ShardKeyClause

case class AllQueryClause[V](
    fname: String,
    vs: java.util.List[V]
) extends IndexableQueryClause[java.util.List[V], Index](
      fname,
      Index,
      CondOps.All -> vs
    ) {}

case class InQueryClause[V](
    fname: String,
    vs: java.util.List[V]
) extends IndexableQueryClause[java.util.List[V], Index](
      fname,
      Index,
      CondOps.In -> vs
    ) {}

case class GtQueryClause[V](fname: String, v: V)
    extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan,
      CondOps.Gt -> v
    ) {}

case class GtEqQueryClause[V](fname: String, v: V)
    extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan,
      CondOps.GtEq -> v
    ) {}

case class LtQueryClause[V](fname: String, v: V)
    extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan,
      CondOps.Lt -> v
    ) {}

case class LtEqQueryClause[V](fname: String, v: V)
    extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan,
      CondOps.LtEq -> v
    ) {}

case class BetweenQueryClause[V](
    fname: String,
    lower: V,
    upper: V
) extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan,
      CondOps.GtEq -> lower,
      CondOps.LtEq -> upper
    ) {}

case class StrictBetweenQueryClause[V](
    fname: String,
    lower: V,
    upper: V
) extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan,
      CondOps.Gt -> lower,
      CondOps.Lt -> upper
    ) {}

case class NeQueryClause[V](fname: String, v: V)
    extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan,
      CondOps.Ne -> v
    ) {}

case class NearQueryClause[V](fname: String, v: V)
    extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan,
      CondOps.Near -> v
    ) {}

case class NearSphereQueryClause[V](
    fname: String,
    lat: Double,
    lng: Double,
    radians: Radians
) extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan
    ) {
  override def extend(q: BasicDBObjectBuilder, signature: Boolean): Unit = {
    q.add(
      CondOps.NearSphere.toString,
      if (signature) 0 else QueryHelpers.list(List(lat, lng))
    )
    q.add(CondOps.MaxDistance.toString, if (signature) 0 else radians.value)
  }
}

case class ModQueryClause[V](
    fname: String,
    v: java.util.List[V]
) extends IndexableQueryClause[java.util.List[V], IndexScan](
      fname,
      IndexScan,
      CondOps.Mod -> v
    ) {}

case class TypeQueryClause(fname: String, v: MongoType.Value)
    extends IndexableQueryClause[Int, IndexScan](
      fname,
      IndexScan,
      CondOps.Type -> v.id
    ) {}

case class ExistsQueryClause(fname: String, v: Boolean)
    extends IndexableQueryClause[Boolean, IndexScan](
      fname,
      IndexScan,
      CondOps.Exists -> v
    ) {}

case class NinQueryClause[V](
    fname: String,
    vs: java.util.List[V]
) extends IndexableQueryClause[java.util.List[V], DocumentScan](
      fname,
      DocumentScan,
      CondOps.Nin -> vs
    ) {}

case class SizeQueryClause(fname: String, v: Int)
    extends IndexableQueryClause[Int, DocumentScan](
      fname,
      DocumentScan,
      CondOps.Size -> v
    ) {}

case class RegexQueryClause[Ind <: MaybeIndexed](
    fname: String,
    actualIB: Ind,
    p: Pattern
) extends IndexableQueryClause[Pattern, Ind](fname, actualIB) {
  val flagMap = Map(
    Pattern.CANON_EQ -> "c",
    Pattern.CASE_INSENSITIVE -> "i",
    Pattern.COMMENTS -> "x",
    Pattern.DOTALL -> "s",
    Pattern.LITERAL -> "t",
    Pattern.MULTILINE -> "m",
    Pattern.UNICODE_CASE -> "u",
    Pattern.UNIX_LINES -> "d"
  )

  def flagsToString(flags: Int) = {
    (for {
      (mask, char) <- flagMap
      if (flags & mask) != 0
    } yield char).mkString
  }

  override def extend(q: BasicDBObjectBuilder, signature: Boolean): Unit = {
    q.add("$regex", if (signature) 0 else p.toString)
    q.add("$options", if (signature) 0 else flagsToString(p.flags))
  }

}

case class RawQueryClause(f: BasicDBObjectBuilder => Unit)
    extends IndexableQueryClause("raw", DocumentScan) {
  override def extend(q: BasicDBObjectBuilder, signature: Boolean): Unit = {
    f(q)
  }
}

case class EmptyQueryClause[V](fname: String)
    extends IndexableQueryClause[V, Index](fname, Index) {
  override def extend(q: BasicDBObjectBuilder, signature: Boolean): Unit = {}
}

case class EqClause[V, Ind <: MaybeIndexed](
    fname: String,
    value: V
) extends IndexableQueryClause[V, Index](fname, Index) {
  override def extend(q: BasicDBObjectBuilder, signature: Boolean): Unit = {
    q.add(fieldName, if (signature) 0 else value)
  }
}

case class WithinCircleClause[V](
    fname: String,
    lat: Double,
    lng: Double,
    radius: Double
) extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan
    ) {
  override def extend(q: BasicDBObjectBuilder, signature: Boolean): Unit = {
    val value =
      if (signature) 0
      else QueryHelpers.list(List(QueryHelpers.list(List(lat, lng)), radius))
    q.push("$within").add("$center", value).pop
  }
}

case class WithinBoxClause[V](
    fname: String,
    lat1: Double,
    lng1: Double,
    lat2: Double,
    lng2: Double
) extends IndexableQueryClause[V, PartialIndexScan](
      fname,
      PartialIndexScan
    ) {
  override def extend(q: BasicDBObjectBuilder, signature: Boolean): Unit = {
    val value =
      if (signature) 0
      else {
        QueryHelpers.list(
          List(QueryHelpers.list(lat1, lng1), QueryHelpers.list(lat2, lng2))
        )
      }
    q.push("$within").add("$box", value).pop
  }
}

case class ElemMatchWithPredicateClause[V](
    fname: String,
    clauses: Seq[QueryClause[_]]
) extends IndexableQueryClause[V, DocumentScan](fname, DocumentScan) {
  override def extend(q: BasicDBObjectBuilder, signature: Boolean): Unit = {
    import io.fsq.rogue.MongoHelpers.AndCondition
    val nested = q.push("$elemMatch")
    MongoHelpers.MongoBuilder.buildCondition(
      AndCondition(clauses.toList, None, None),
      nested,
      signature
    )
    nested.pop
  }
}

class ModifyClause(val operator: ModOps.Value, fields: (String, _)*) {
  def extend(q: BasicDBObjectBuilder): Unit = {
    fields foreach { case (name, value) => q.add(name, value) }
  }
}

class ModifyAddEachClause(fieldName: String, values: Iterable[_])
    extends ModifyClause(ModOps.AddToSet) {
  override def extend(q: BasicDBObjectBuilder): Unit = {
    q.push(fieldName).add("$each", QueryHelpers.list(values)).pop
  }
}

class ModifyPushEachClause(fieldName: String, values: Iterable[_])
    extends ModifyClause(ModOps.Push) {
  override def extend(q: BasicDBObjectBuilder): Unit = {
    q.push(fieldName).add("$each", QueryHelpers.list(values)).pop
  }
}

class ModifyPushEachSliceClause(
    fieldName: String,
    slice: Int,
    values: Iterable[_]
) extends ModifyClause(ModOps.Push) {
  override def extend(q: BasicDBObjectBuilder): Unit = {
    q.push(fieldName)
      .add("$each", QueryHelpers.list(values))
      .add("$slice", slice)
      .pop
  }
}

class ModifyPushEachSlicePositionClause(
    fieldName: String,
    slice: Int,
    position: Int,
    values: Iterable[_]
) extends ModifyClause(ModOps.Push) {
  override def extend(q: BasicDBObjectBuilder): Unit = {
    q.push(fieldName)
      .add("$each", QueryHelpers.list(values))
      .add("$slice", slice)
      .add("$position", position)
      .pop
  }
}

class ModifyBitClause(fieldName: String, value: Int, op: BitOps.Value)
    extends ModifyClause(ModOps.Bit) {
  override def extend(q: BasicDBObjectBuilder): Unit = {
    q.push(fieldName).add(op.toString, value).pop
  }
}

class ModifyPullWithPredicateClause[V](
    fieldName: String,
    clauses: Seq[QueryClause[_]]
) extends ModifyClause(ModOps.Pull) {
  override def extend(q: BasicDBObjectBuilder): Unit = {
    import io.fsq.rogue.MongoHelpers.AndCondition
    MongoHelpers.MongoBuilder.buildCondition(
      AndCondition(clauses.toList, None, None),
      q,
      false
    )
  }
}

class ModifyPullObjWithPredicateClause[V](
    fieldName: String,
    clauses: Seq[QueryClause[_]]
) extends ModifyClause(ModOps.Pull) {
  override def extend(q: BasicDBObjectBuilder): Unit = {
    import io.fsq.rogue.MongoHelpers.AndCondition
    val nested = q.push(fieldName)
    MongoHelpers.MongoBuilder.buildCondition(
      AndCondition(clauses.toList, None, None),
      nested,
      false
    )
    nested.pop
  }
}
