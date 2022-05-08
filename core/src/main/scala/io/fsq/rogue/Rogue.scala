// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.

package io.fsq.rogue

import com.mongodb.DBObject
import io.fsq.field.{
  Field => RField,
  OptionalField => ROptionalField,
  RequiredField => RRequiredField
}
import java.util.Date
import org.bson.types.ObjectId
import io.fsq.rogue.enums.EnumInstance

/** A utility trait containing typing shorthands, and a collection of implicit
  * conversions that make query syntax much simpler.
  *
  * @see
  *   AbstractQuery for an example of the use of implicit conversions.
  */
trait Rogue {

  // QueryField implicits
  implicit def rbooleanFieldtoQueryField[M](
      f: RField[Boolean, M]
  ): QueryField[Boolean, M] = new QueryField(f)
  implicit def rcharFieldtoQueryField[M](
      f: RField[Char, M]
  ): QueryField[Char, M] = new QueryField(f)

  implicit def rbyteFieldtoNumericQueryField[M](
      f: RField[Byte, M]
  ): NumericQueryField[Byte, M] = new NumericQueryField(f)
  implicit def rshortFieldtoNumericQueryField[M](
      f: RField[Short, M]
  ): NumericQueryField[Short, M] = new NumericQueryField(f)
  implicit def rintFieldtoNumericQueryField[M](
      f: RField[Int, M]
  ): NumericQueryField[Int, M] = new NumericQueryField(f)
  implicit def rlongFieldtoNumericQueryField[F <: Long, M](
      f: RField[F, M]
  ): NumericQueryField[F, M] = new NumericQueryField(f)
  implicit def rjlongFieldtoNumericQueryField[F <: java.lang.Long, M](
      f: RField[F, M]
  ): NumericQueryField[F, M] = new NumericQueryField(f)
  implicit def rfloatFieldtoNumericQueryField[M](
      f: RField[Float, M]
  ): NumericQueryField[Float, M] = new NumericQueryField(f)
  implicit def rdoubleFieldtoNumericQueryField[M](
      f: RField[Double, M]
  ): NumericQueryField[Double, M] = new NumericQueryField(f)

  implicit def rstringFieldToStringQueryField[F <: String, M](
      f: RField[F, M]
  ): StringQueryField[F, M] = new StringQueryField(f)
  
  implicit def robjectIdFieldToObjectIdQueryField[F <: ObjectId, M](
      f: RField[F, M]
  ): ObjectIdQueryField[F, M] = new ObjectIdQueryField[F, M](f)
  
  implicit def rdateFieldToDateQueryField[M](
      f: RField[Date, M]
  ): DateQueryField[M] = new DateQueryField(f)
  implicit def rdbobjectFieldToQueryField[M](
      f: RField[DBObject, M]
  ): QueryField[DBObject, M] = new QueryField(f)

  implicit def renumNameFieldToEnumNameQueryField[M, E <: Enumeration, F <: Enumeration#Value](
      f: RField[F, M] with EnumInstance[E]
  ): EnumNameQueryField[M, E, F] = new EnumNameQueryField(f)
  implicit def renumerationSeqFieldToEnumerationSeqQueryField[
      M,
      F <: Enumeration#Value,
      CC[_] <: Seq[_]
  ](f: RField[CC[F], M] with EnumInstance[_]): EnumerationSeqQueryField[F, M, CC] =
    new EnumerationSeqQueryField[F, M, CC](f)
  implicit def rlatLongFieldToGeoQueryField[M](
      f: RField[LatLong, M]
  ): GeoQueryField[M] = new GeoQueryField(f)
  implicit def rStringsSeqFieldToStringsSeqQueryField[M, CC[String] <: Seq[
    String
  ]](f: RField[CC[String], M]): StringsSeqQueryField[M, CC] =
    new StringsSeqQueryField[M, CC](f)
  implicit def rseqFieldToSeqQueryField[M, F: BSONType, ST[F] <: Seq[F]](
      f: RField[ST[F], M]
  ): SeqQueryField[F, M, ST] = new SeqQueryField[F, M, ST](f)
  implicit def rmapFieldToMapQueryField[M, F](
      f: RField[Map[String, F], M]
  ): MapQueryField[F, M] = new MapQueryField[F, M](f)

  /** ModifyField implicits
    *
    * These are dangerous in the general case, unless the field type can be
    * safely serialized or the field class handles necessary serialization. We
    * specialize some safe cases.
    */
  implicit def rfieldToSafeModifyField[M, F](
      f: RField[F, M]
  ): SafeModifyField[F, M] = new SafeModifyField(f)
  implicit def booleanRFieldToModifyField[M](
      f: RField[Boolean, M]
  ): ModifyField[Boolean, M] = new ModifyField(f)
  implicit def charRFieldToModifyField[M](
      f: RField[Char, M]
  ): ModifyField[Char, M] = new ModifyField(f)

  implicit def byteRFieldToModifyField[M](
      f: RField[Byte, M]
  ): NumericModifyField[Byte, M] = new NumericModifyField(f)
  implicit def shortRFieldToModifyField[M](
      f: RField[Short, M]
  ): NumericModifyField[Short, M] = new NumericModifyField(f)
  implicit def intRFieldToModifyField[M](
      f: RField[Int, M]
  ): NumericModifyField[Int, M] = new NumericModifyField(f)
  implicit def longRFieldToModifyField[M, F <: Long](
      f: RField[F, M]
  ): NumericModifyField[F, M] = new NumericModifyField(f)
  implicit def floatRFieldToModifyField[M](
      f: RField[Float, M]
  ): NumericModifyField[Float, M] = new NumericModifyField(f)
  implicit def doubleRFieldToModifyField[M](
      f: RField[Double, M]
  ): NumericModifyField[Double, M] = new NumericModifyField(f)

  implicit def stringRFieldToModifyField[M, F <: String](
      f: RField[F, M]
  ): ModifyField[F, M] = new ModifyField(f)
  implicit def objectidRFieldToModifyField[M, F <: ObjectId](
      f: RField[F, M]
  ): ModifyField[F, M] = new ModifyField(f)
  implicit def dateRFieldToDateModifyField[M](
      f: RField[Date, M]
  ): DateModifyField[M] = new DateModifyField(f)

  implicit def renumerationFieldToEnumerationModifyField[
      M,
      F <: Enumeration#Value
  ](f: RField[F, M]): EnumerationModifyField[M, F] =
    new EnumerationModifyField(f)

  implicit def renumerationSeqFieldToEnumerationSeqModifyField[
      M,
      F <: Enumeration#Value,
      CC[_] <: Seq[_]
  ](f: RField[CC[F], M]): EnumerationSeqModifyField[F, M, CC] =
    new EnumerationSeqModifyField[F, M, CC](f)

  implicit def rlatLongFieldToGeoQueryModifyField[M](
      f: RField[LatLong, M]
  ): GeoModifyField[M] =
    new GeoModifyField(f)

  implicit def rSeqFieldToSeqModifyField[M, F: BSONType, CC[F] <: Seq[F]](
      f: RField[CC[F], M]
  ): SeqModifyField[F, M, CC] =
    new SeqModifyField[F, M, CC](f)

  implicit def rArrayFieldToArrayModifyField[M, F: BSONType](
      f: RField[Array[F], M]
  ): ArrayModifyField[F, M] =
    new ArrayModifyField[F, M](f)

  implicit def rmapFieldToMapModifyField[M, F](
      f: RField[Map[String, F], M]
  ): MapModifyField[F, M] =
    new MapModifyField[F, M](f)

  // SelectField implicits
  implicit def roptionalFieldToSelectField[M, V](
      f: ROptionalField[V, M]
  ): SelectField[Option[V], M] = new OptionalSelectField(f)

  implicit def rrequiredFieldToSelectField[M, V](
      f: RRequiredField[V, M]
  ): SelectField[V, M] = new MandatorySelectField(f)

  class Flattened[A, B]
  implicit def anyValIsFlattened[A <: AnyVal]: Flattened[A, A] =
    new Flattened[A, A]
  implicit def enumIsFlattened[A <: Enumeration#Value]: Flattened[A, A] =
    new Flattened[A, A]
  implicit val stringIsFlattened:Flattened[String, String] = new Flattened[String, String]
  implicit val objectIdIsFlattened:Flattened[ObjectId, ObjectId] = new Flattened[ObjectId, ObjectId]
  implicit val dateIsFlattened:Flattened[java.util.Date, java.util.Date] = new Flattened[java.util.Date, java.util.Date]
  implicit def recursiveFlattenList[A, B](implicit ev: Flattened[A, B]):Flattened[List[A], B] =
    new Flattened[List[A], B]
  implicit def recursiveFlattenVector[A, B](implicit ev: Flattened[A, B]):Flattened[Vector[A], B] =
    new Flattened[Vector[A], B]
  implicit def recursiveFlattenSeq[A, B](implicit ev: Flattened[A, B]):Flattened[Seq[A], B] =
    new Flattened[Seq[A], B]
  implicit def recursiveFlattenArray[A, B](implicit ev: Flattened[A, B]):Flattened[Array[A], B] =
    new Flattened[Array[A], B]

}

object Rogue extends Rogue
