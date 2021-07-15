package me.sgrouples.rogue.cc

import scala.concurrent.Future

private[cc] object Waiter {
  def waitForFuture[T](f: Future[T]): T = {
    scala.concurrent.Await.result(f, scala.concurrent.duration.Duration.Inf)
  }
}
