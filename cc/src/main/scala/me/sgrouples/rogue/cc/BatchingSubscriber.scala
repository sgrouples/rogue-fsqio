package me.sgrouples.rogue.cc

import org.bson.BsonDocument
import org.reactivestreams.{Subscriber, Subscription}

import scala.concurrent.Future

//TODO - doesn't work at all now, maybe not needed, as Source /publisher is better anyways
class BatchingSubscriber[T, R](batchSize: Int,
                               serializer: RogueBsonRead[R],
                               f: Seq[R] => Future[Seq[T]]) extends Subscriber[BsonDocument] {

  override def onSubscribe(s: Subscription): Unit = {
    s.request(batchSize)
  }

  override def onNext(t: BsonDocument): Unit = {
    val r = serializer.fromDocument(t)
  }

  override def onError(t: Throwable): Unit = ???

  override def onComplete(): Unit = ???

  def toFuture(): Future[Seq[T]] = ???
}
