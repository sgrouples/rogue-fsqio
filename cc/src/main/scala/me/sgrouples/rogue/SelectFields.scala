package me.sgrouples.rogue

import io.fsq.field.Field
import io.fsq.rogue._
import me.sgrouples.rogue.cc.CcMeta
import org.bson.BsonValue


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

class CClassSeqQueryField[C <: Product, M <: CcMeta[C], O](fld: CClassListField[C, M , O], owner:O) //, toBson: B => BsonValue)
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

  def elemMatch[V](clauseFuncs: (O => QueryClause[_])*) = {
    new ElemMatchWithPredicateClause(
      field.name,
      clauseFuncs.map(cf => cf(fld.owner))
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
class CClassQueryField[C <: Product, M <: CcMeta[C], O](fld: CClassField[C, M,O], owner:O) extends AbstractQueryField[C, C, BsonValue, O](fld){
  override def valueToDB(v: C): BsonValue = fld.childMeta.write(v)
  def subfield[V](f: M => Field[V,M]): SelectableDummyField[V, O] = {
    new SelectableDummyField[V, O](fld.name+"." + f(fld.childMeta).name, owner)
  }
  def subselect[V](f: M => Field[V, M]): SelectableDummyField[V, O] = subfield(f)
}

class OptCClassQueryField[C <: Product, M <: CcMeta[C], O](fld: OptCClassField[C, M, O], owner:O) extends AbstractQueryField[C, C, BsonValue, O](fld){
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


