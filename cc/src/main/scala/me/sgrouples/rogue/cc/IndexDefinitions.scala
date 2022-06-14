package me.sgrouples.rogue.cc

import io.fsq.rogue.index.{IndexBuilder, MongoIndex}
import me.sgrouples.rogue.cc.macros.MCcMeta
import org.mongodb.scala.MongoDatabase
import org.mongodb.scala.model.IndexOptions

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait IndexDefinitions[RecordType, OwnerType <: CcMeta[RecordType]]
    extends IndexBuilder[OwnerType] {

  this: MCcMeta[RecordType, OwnerType] =>

  val indexes: Seq[Idx]

  def tryCreateDefinedIndexes(implicit
      mongoDatabase: MongoDatabase
  ): Seq[Try[String]] =
    indexes.map(el => Try(createIndex(el.idx, el.options)))

  def createDefinedIndexes(implicit
      mongoDatabase: MongoDatabase
  ): Seq[String] =
    indexes.map(el => createIndex(el.idx, el.options))

  def createDefinedIndexesAsync(implicit
      mongoDatabase: MongoDatabase,
      executionContext: ExecutionContext
  ): Future[List[String]] = {
    indexes.foldLeft(Future.successful(List.empty[String])) {
      case (f, Idx(index, options)) =>
        f.flatMap(list => createIndexAsync(index, options).map(_ :: list))
    }
  }

  def tryCreateDefinedIndexesAsync(implicit
      mongoDatabase: MongoDatabase,
      executionContext: ExecutionContext
  ): Future[Seq[Try[String]]] = {
    indexes.foldLeft(Future.successful(List.empty[Try[String]])) {
      case (f, Idx(index, options)) =>
        f.flatMap { list =>
          createIndexAsync(index, options)
            .transformWith {
              case Failure(exception) => Future.successful(Failure(exception))
              case Success(str)       => Future.successful(Success(str))
            }
            .map(_ :: list)
        }
    }
  }

  case class Idx(
      idx: MongoIndex[OwnerType],
      options: IndexOptions = new IndexOptions
  )
}
