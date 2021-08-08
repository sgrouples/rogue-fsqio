package me.sgrouples.rogue.cc

import org.bson.BsonDocument
import org.reactivestreams.{Subscriber, Subscription}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

class BatchingSubscriber[T, R](
    batchSize: Int,
    f: Seq[R] => Future[Seq[T]]
)(implicit ec: ExecutionContext)
    extends Subscriber[R] {
  val buffer = ListBuffer[R]()
  val resultAcc = IndexedSeq.newBuilder[T]
  val p = Promise[Seq[T]]()
  var sub: Subscription = null

  override def onSubscribe(s: Subscription): Unit = {
    sub = s
    sub.request(batchSize)
  }

  override def onNext(r: R): Unit = {
    buffer.addOne(r)
    if (buffer.size == batchSize) {
      tryCallF().map { _ =>
        sub.request(batchSize)
      }
    }
  }

  override def onError(t: Throwable): Unit = {
    buffer.clear()
    Option(sub).map(_.cancel())
    p.failure(t)
  }

  override def onComplete(): Unit = {
    if (!buffer.isEmpty) {
      tryCallF().map { _ =>
        p.success(resultAcc.result())
        Option(sub).map(_.cancel())
      }
    } else {
      p.success(resultAcc.result())
      Option(sub).map(_.cancel())
    }
  }

  private[this] def tryCallF(): Future[Unit] = {
    try {
      f(buffer.result())
        .map { result =>
          resultAcc.addAll(result)
          buffer.clear()
        }
        .recover { case NonFatal(e) =>
          Option(sub).map(_.cancel())
          p.failure(e)
          Future.failed(e)
        }
    } catch {
      case NonFatal(e) =>
        Option(sub).map(_.cancel())
        p.failure(e)
        Future.failed(e)
    }
  }

  def toFuture(): Future[Seq[T]] = p.future
}
