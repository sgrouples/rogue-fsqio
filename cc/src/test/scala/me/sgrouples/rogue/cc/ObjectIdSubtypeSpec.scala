package me.sgrouples.rogue.cc

import java.util.UUID

import io.fsq.rogue._
import me.sgrouples.rogue.cc.macros.*
import me.sgrouples.rogue.cc.macros.MacroBsonFormatAuto.*
import me.sgrouples.rogue.cc
import org.bson.types.ObjectId
import munit.FunSuite
import com.softwaremill.tagging.*
import me.sgrouples.rogue.cc._
import me.sgrouples.rogue.cc.CcRogue._

trait HasId[T] {
  type Id

}

trait TypedObjectId[RecordType, TagType] {

  type Id = ObjectId @@ TagType

  object Id {

    def get(): Id = new ObjectId().taggedWith[TagType]

    def apply(text: String): Id = new ObjectId(text).taggedWith[TagType]

    object Extract {
      def unapply(in: String): Option[Id] = try { Some(apply(in)) }
      catch { case e: IllegalArgumentException => None }
      def unapply(inOpt: Option[String]): Option[Option[Id]] = try {
        Some(inOpt.map(apply))
      } catch { case e: IllegalArgumentException => None }
    }

  }

  implicit object hasId extends HasId[RecordType] {
    type Id = ObjectId @@ TagType
  }

  implicit object idOrdering extends Ordering[Id] {
    override def compare(x: Id, y: Id): Int = x.compareTo(y)
  }
}

case class A(_id: A.Id, b: String)

object A extends TypedObjectId[A, A]

class ObjectIdSubtypeSpec extends FunSuite {

  class MetaA extends MCcMeta[A, MetaA]() {
    val id = ObjectIdSubtypeField[A.Id]("_id")
  }
  val X = new MetaA
  val t: Query[
    MetaA,
    A.Id,
    Unordered with Unlimited with Unskipped with HasNoOrClause with Unhinted with ShardKeyNotSpecified with SelectedOne
  ] = X.select(_.id)

  test("t should compile") {
    //should compile ..
    val t: Query[_, A.Id, _] = X.select(_.id)
  }
}

trait TypedStringId[RecordType, TagType] {

  private type IdType = String @@ TagType

  type Id = IdType

  object Id {

    def get(): Id =
      apply(UUID.randomUUID().toString())

    def apply(text: String): Id =
      text.taggedWith[TagType]

    object Extract {
      def unapply(in: String): Option[Id] = try { Some(apply(in)) }
      catch { case e: IllegalArgumentException => None }
      def unapply(inOpt: Option[String]): Option[Option[Id]] = try {
        Some(inOpt.map(apply))
      } catch { case e: IllegalArgumentException => None }
    }
  }

  implicit object hasId extends HasId[RecordType] {
    type Id = IdType
  }

  implicit object idOrdering extends Ordering[Id] {
    override def compare(x: Id, y: Id): Int = x.compareTo(y)
  }
}

case class B(id: B.Id)

object B extends TypedStringId[B, B]

class StringTaggedSpec extends FunSuite {

  class MetaB extends MCcMeta[B, MetaB]() {
    val id = StringSubtypeField[B.Id]("id")
  }

  val X = new MetaB
  val id: B.Id = B.Id.get()
  val t: Query[MetaB, B.Id, _] = X.select(_.id).where(_.id eqs id)

  test("t should compile") {
    val t: Query[_, B.Id, _] =
      X.select(_.id).where(_.id eqs id)
  }
}
