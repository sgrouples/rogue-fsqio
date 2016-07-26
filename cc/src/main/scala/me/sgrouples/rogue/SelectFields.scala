package me.sgrouples.rogue

import java.time.{LocalDateTime, ZoneOffset}
import java.util.Date

import io.fsq.field.Field
import io.fsq.rogue._
import me.sgrouples.rogue.cc.CcMeta
import org.bson.{BsonDateTime, BsonDocument, BsonValue}


/**
  * Trait representing a field and all the operations on it.
  * 
  * @tparam F the underlying type of the field
  * @tparam V the type of values allowed to be compared to the field
  * @tparam DB the type V is converted into in the BSON representation of the field
  * @tparam M the type of the owner of the field
  */
//abstract class AbstractQueryField[F, V, DB, M](val field: Field[F, M]) {
/*
abstract class AbstractListQueryField[F, V, DB, M, CC[X] <: Seq[X]](field: Field[CC[F], M])
    extends AbstractQueryField[CC[F], V, DB, M](field) {

 */
//F - value type, C
//
// M - meta
// DB = Bson
// CC - container type, List
// - B -
//abstract class AbstractListQueryField[F, V, DB, M, CC[X] <: Seq[X]](field: Field[CC[F], M])

class CClassSeqQueryField[C , M <: CcMeta[C], O](fld: CClassListField[C, M , O], owner:O) //, toBson: B => BsonValue)
  extends AbstractListQueryField[C, C, BsonValue, O, Seq](fld) {
  override def valueToDB(c: C) = fld.childMeta.write(c)

  def subfield[V, V1](f: M => Field[V, M])(implicit ev: Rogue.Flattened[V, V1]): SelectableDummyField[List[V1], O] = {
    new SelectableDummyField[List[V1], O](fld.name + "." + f(fld.childMeta).name, owner)
  }

  def subselect[V, V1](f: M => Field[V, M])(implicit ev: Rogue.Flattened[V, V1]): SelectField[Option[List[V1]], O] = {
    Rogue.roptionalFieldToSelectField[O, List[V1]](subfield(f))
  }

  def unsafeField[V](name: String): DummyField[V, O] = {
    new DummyField[V, O](field.name + "." + name, fld.owner)
  }

  def elemMatch[V](clauseFuncs: (M => QueryClause[_])*) = {
    new ElemMatchWithPredicateClause(
      field.name,
      clauseFuncs.map(cf => cf(fld.childMeta))
    )
  }
}




/*

 */




/*


class BsonRecordListQueryField[M, B](field: Field[List[B], M], rec: B, asDBObject: B => DBObject)
    extends AbstractListQueryField[B, B, DBObject, M, List](field) {
  override def valueToDB(b: B) = asDBObject(b)

  def subfield[V, V1](f: B => Field[V, B])(implicit ev: Rogue.Flattened[V, V1]): SelectableDummyField[List[V1], M] = {
    new SelectableDummyField[List[V1], M](field.name + "." + f(rec).name, field.owner)
  }

  def subselect[V, V1](f: B => Field[V, B])(implicit ev: Rogue.Flattened[V, V1]): SelectField[Option[List[V1]], M] = {
    Rogue.roptionalFieldToSelectField(subfield(f))
  }

  def unsafeField[V](name: String): DummyField[V, M] = {
    new DummyField[V, M](field.name + "." + name, field.owner)
  }

  def elemMatch[V](clauseFuncs: (B => QueryClause[_])*) = {
    new ElemMatchWithPredicateClause(
      field.name,
      clauseFuncs.map(cf => cf(rec))
    )
  }
}
 */
class CClassQueryField[C, M <: CcMeta[C], O](fld: CClassField[C, M,O], owner:O) extends AbstractQueryField[C, C, BsonValue, O](fld){
  override def valueToDB(v: C): BsonValue = {
    val x = fld.childMeta.write(v)
    println(s"writing value ${v} to db as ${x}")
    x
  }
  def subfield[V](f: M => Field[V,M]): SelectableDummyField[V, O] = {
    new SelectableDummyField[V, O](fld.name+"." + f(fld.childMeta).name, owner)
  }
  def subselect[V](f: M => Field[V, M]): SelectableDummyField[V, O] = subfield(f)
}

class CClassLikeQueryField[C, M <: CcMeta[C], O](fld: Field[C, O], meta:M, owner:O) extends AbstractQueryField[C, C, BsonValue, O](fld){
  override def valueToDB(v: C): BsonValue = {
    val x = meta.write(v)
    println(s"writing value ${v} to db as ${x}")
    x
  }
  def subfield[V](f: M => Field[V,M]): SelectableDummyField[V, O] = {
    new SelectableDummyField[V, O](fld.name+"." + f(meta).name, owner)
  }
  def subselect[V](f: M => Field[V, M]): SelectableDummyField[V, O] = subfield(f)
}



class OptCClassQueryField[C , M <: CcMeta[C], O](fld: OptCClassField[C, M, O], owner:O) extends AbstractQueryField[C, C, BsonValue, O](fld){
  override def valueToDB(v: C): BsonValue = fld.childMeta.write(v)
  def subfield[V](f: M => Field[V,M]): SelectableDummyField[V, O] = {
    new SelectableDummyField[V, O](fld.name+"." + f(fld.childMeta).name, owner)
  }
  def subselect[V](f: M => Field[V, M]): SelectableDummyField[V, O] = {
    val r = subfield(f)
    println(s"R ${r}")
    r
  }
}

class LocalDateTimeQueryField[M](field: Field[LocalDateTime, M])
extends AbstractQueryField[LocalDateTime, LocalDateTime, Date, M](field) {
  import LocalDateTimeToMongo._
  override def valueToDB(d: LocalDateTime) = ldtToDate(d)

  def before(d: LocalDateTime) = new LtQueryClause(field.name, ldtToDate(d))
  def after(d: LocalDateTime) = new GtQueryClause(field.name, ldtToDate(d))
  def onOrBefore(d: LocalDateTime) = new LtEqQueryClause(field.name, ldtToDate(d))
  def onOrAfter(d: LocalDateTime) = new GtEqQueryClause(field.name, ldtToDate(d))


}

class CClassModifyField[C , M <: CcMeta[C], O](fld: CClassField[C, M, O]) extends AbstractModifyField[C, BsonDocument, O](fld) {
  override def valueToDB(b: C):BsonDocument = fld.childMeta.write(b).asDocument()
}

class OptCClassModifyField[C , M <: CcMeta[C], O](fld: OptCClassField[C, M, O]) extends AbstractModifyField[C, BsonDocument, O](fld) {
  override def valueToDB(b: C):BsonDocument = fld.childMeta.write(b).asDocument()
}


class CClassSeqModifyField[C , M <: CcMeta[C], O](fld: CClassListField[C, M, O])
  extends AbstractListModifyField[C, BsonDocument, O, Seq](fld) {
  override def valueToDB(b: C):BsonDocument = fld.childMeta.write(b).asDocument()

  def pullObjectWhere(clauseFuncs: (M => QueryClause[_])*) = {
    new ModifyPullObjWithPredicateClause(
      field.name,
      clauseFuncs.map(cf => cf(fld.childMeta))
    )
  }

  override def $ = new SelectableDummyCCField[C, M, O](fld.name + ".$", fld.childMeta, fld.owner)

}

class SelectableDummyCCField[V, M <: CcMeta[V], O](name: String, val meta:M, owner:O) extends SelectableDummyField[V, O](name, owner)

/*
class CClassSeqQueryField[C <: Product, M <: CcMeta[C], O](fld: CClassListField[C, M , O], owner:O) //, toBson: B => BsonValue)
  extends AbstractListQueryField[C, C, BsonValue, O, Seq](fld) {
  override def valueToDB(c: C) = fld.childMeta.write(c)


class BsonRecordListModifyField[M, B](field: Field[List[B], M], rec: B, asDBObject: B => DBObject)(implicit mf: Manifest[B])
    extends AbstractListModifyField[B, DBObject, M, List](field) {
  override def valueToDB(b: B) = asDBObject(b)

  // override def $: BsonRecordField[M, B] = {
  //   new BsonRecordField[M, B](field.owner, rec.meta)(mf) {
  //     override def name = field.name + ".$"
  //   }
  // }


}

 */



/*
class BsonRecordModifyField[M, B](field: Field[B, M], asDBObject: B => DBObject)
    extends AbstractModifyField[B, DBObject, M](field) {
  override def valueToDB(b: B) = asDBObject(b)
}

 */

//class OptCClassModifyField[C <: Product, M <: CcMeta[C], O](fld: OptCClassField[C, M, O]) extends AbstractModifyField[Option[C], BsonDocument, O](fld) {

class LocalDateTimeModifyField[O](field:LocalDateTimeField[O]) extends AbstractModifyField[LocalDateTime, Date, O](field) {
  import LocalDateTimeToMongo._
  override def valueToDB(d: LocalDateTime) = ldtToDate(d)
  def currentDate = new ModifyClause(ModOps.CurrentDate, field.name -> true)
}


object LocalDateTimeToMongo {
  final def ldtToDate(d: LocalDateTime): Date = Date.from(d.toInstant(ZoneOffset.UTC))
}