package me.sgrouples.rogue.cc

trait BsonExecutors[MB] {
  def async: AsyncBsonQueryExecutor[MB]
}

object CcBsonExecutors extends BsonExecutors[CcMeta[_]] {
  override val async = CcAsyncQueryExecutor
}