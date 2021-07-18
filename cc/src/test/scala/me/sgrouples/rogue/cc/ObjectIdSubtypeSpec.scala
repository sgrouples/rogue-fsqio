package me.sgrouples.rogue.cc

import java.util.UUID

import io.fsq.rogue._
import me.sgrouples.rogue.BsonFormats._
import me.sgrouples.rogue.cc
import org.bson.types.ObjectId
import munit.FunSuite
import shapeless.tag
import shapeless.tag.@@
import me.sgrouples.rogue.cc._
import me.sgrouples.rogue.cc.CcRogue._

trait HasId[T] {
  type Id

}

trait TypedObjectId[RecordType, TagType] {

  type Id = ObjectId @@ TagType

  object Id {

    def get(): Id = tag[TagType][ObjectId](new ObjectId)

    def apply(text: String): Id = tag[TagType][ObjectId](new ObjectId(text))

    object Extract {
      def unapply(in: String): Option[Id] = try { Some(apply(in)) } catch { case e: IllegalArgumentException => None }
      def unapply(inOpt: Option[String]): Option[Option[Id]] = try { Some(inOpt.map(apply)) } catch { case e: IllegalArgumentException => None }
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

  class MetaA extends RCcMetaExt[A, MetaA]() {
    val id = ObjectIdSubtypeField[A.Id]("_id")
  }
  val X = new MetaA
  val t: Query[MetaA, cc.A.Id, Unordered with Unlimited with Unskipped with HasNoOrClause with Unhinted with ShardKeyNotSpecified with SelectedOne] = X.select(_.id)

  test("t should compile") {
    //should compile ..
    val t: Query[_, me.sgrouples.rogue.cc.A.Id, _] = X.select(_.id)
  }
}

trait TypedStringId[RecordType, TagType] {

  private type IdType = String @@ TagType

  type Id = IdType

  object Id {

    def get(): Id =
      apply(UUID.randomUUID().toString())

    def apply(text: String): Id =
      tag[TagType][String](text)

    object Extract {
      def unapply(in: String): Option[Id] = try { Some(apply(in)) } catch { case e: IllegalArgumentException => None }
      def unapply(inOpt: Option[String]): Option[Option[Id]] = try { Some(inOpt.map(apply)) } catch { case e: IllegalArgumentException => None }
    }
  }

  implicit object hasId extends HasId[RecordType] {
    type Id = IdType
  }

  implicit object idOrdering extends Ordering[Id] {
    override def compare(x: Id, y: Id): Int = x.compareTo(y)
  }
}

object B extends TypedStringId[B, B]

case class B(id: B.Id)

class StringTaggedSpec extends FunSuite {

  class MetaB extends RCcMetaExt[B, MetaB]() {
    val id = StringTaggedField[B]("id")
  }

  val X = new MetaB
  val id: cc.B.Id = cc.B.Id.get()
  val t: Query[MetaB, cc.B.Id, _] = X.select(_.id).where(_.id eqs id)

  test("t should compile") {
    val t: Query[_, me.sgrouples.rogue.cc.B.Id, _] = X.select(_.id).where(_.id eqs id)
  }
}
