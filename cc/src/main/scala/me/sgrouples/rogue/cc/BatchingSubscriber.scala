package me.sgrouples.rogue.cc

import org.reactivestreams.{Subscriber, Subscription}

import scala.concurrent.Future
import org.mongodb.scala.Document

class BatchingSubscriber[T, R](batchSize: Int,
                               serializer: RogueBsonRead[R],
                               f: Seq[R] => Future[Seq[T]]) extends Subscriber[Document] {

  override def onSubscribe(s: Subscription): Unit = {
    s.request(batchSize)
  }

  override def onNext(t: Document): Unit = {
    val r = serializer.fromDocument(t)
  }

  override def onError(t: Throwable): Unit = ???

  override def onComplete(): Unit = ???

  def toFuture(): Future[Seq[T]]
}
