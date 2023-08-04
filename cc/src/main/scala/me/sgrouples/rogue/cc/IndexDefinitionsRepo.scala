package me.sgrouples.rogue.cc

import me.sgrouples.rogue.cc.macros.MCcMeta
import org.mongodb.scala.MongoDatabase

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait IndexDefinitionsRepo {
  implicit def db: MongoDatabase

  def meta: MCcMeta[_, _] with IndexDefinitions[_, _]

  def tryCreateDefinedIndexes(): Seq[Try[String]] = meta.tryCreateDefinedIndexes

  def createDefinedIndexes(): Seq[String] = meta.createDefinedIndexes

  def createDefinedIndexesAsync(implicit
      executionContext: ExecutionContext = ExecutionContext.global
  ): Future[List[String]] =
    meta.createDefinedIndexesAsync

  def tryCreateDefinedIndexesAsync(implicit
      executionContext: ExecutionContext = ExecutionContext.global
  ): Future[Seq[Try[String]]] =
    meta.tryCreateDefinedIndexesAsync
}
