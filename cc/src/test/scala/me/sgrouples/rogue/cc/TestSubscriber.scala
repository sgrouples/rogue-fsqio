package me.sgrouples.rogue.cc

import java.util.concurrent.{CountDownLatch, TimeUnit}

import org.reactivestreams.{Subscriber, Subscription}

import scala.collection.mutable.ListBuffer

class TestSubscriber extends Subscriber[AnyRef] {
  val latch = new CountDownLatch(1)
  val recieved = new ListBuffer[AnyRef]()
  var x: Subscription = null
  override def onSubscribe(s: Subscription): Unit = {
    x = s
    s.request(Int.MaxValue)
  }

  override def onNext(t: AnyRef): Unit = {
    recieved += t
  }

  override def onError(t: Throwable): Unit = {
    onComplete()
  }

  override def onComplete(): Unit = {
    latch.countDown()
  }

  def getRecieved(): List[AnyRef] = recieved.result()

  def waitForAll(): Unit = {
    import com.mongodb.MongoTimeoutException
    if (!(latch.await(5, TimeUnit.SECONDS))) {
      throw new MongoTimeoutException("Publisher onComplete timed out")
    } else {
      ()
    }
  }
}
