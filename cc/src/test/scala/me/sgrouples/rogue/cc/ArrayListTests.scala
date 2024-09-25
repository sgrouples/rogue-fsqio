package me.sgrouples.rogue.cc

import munit.FunSuite
import org.mongodb.scala.MongoDatabase
import me.sgrouples.rogue.cc.macros._
import me.sgrouples.rogue.cc.macros.MacroBsonFormatAuto.*
import me.sgrouples.rogue.cc.CcRogue._

import scala.concurrent.Future

case class ArrayOfListWrapper(
                               arrayOfList: Array[List[String]],
                               _id: Int = 1
                       )

class ArrayOfListWrapperMeta extends MCcMeta[ArrayOfListWrapper, ArrayOfListWrapperMeta]() {
  val id = IntField("id")
  val arrayOfList = ArrayField[List[String]]("arrayOfList")
}
class ArrayOfListRepository()(implicit db:MongoDatabase) {
  private val Repo = new ArrayOfListWrapperMeta
  def load(): Future[Option[Array[List[String]]]] = Repo.where(_.id eqs 1).select(_.arrayOfList).getAsync()

  def addWords(words: Seq[String]) = {
    Repo.where(_.id eqs 1).modify(_.arrayOfList addToSet words.sorted.toList).upsertOneAsync()
  }

  def removeWords(words: Seq[String]) = {
    Repo.where(_.id eqs 1).modify(_.arrayOfList pull words.sorted.toList).updateOneAsync()
  }
}

class ArrayListTests extends FunSuite {
  test("nested array list"){
    //it should just compile
    implicit val db:MongoDatabase=null

    val repo=new ArrayOfListRepository()
    assert(true)
  }
}
