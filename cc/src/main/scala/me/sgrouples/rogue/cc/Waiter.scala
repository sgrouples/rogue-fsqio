package me.sgrouples.rogue.cc

import scala.concurrent.{Future, blocking}

private[cc] object Waiter {
  def waitForFuture[T](f: Future[T]): T = blocking {
    scala.concurrent.Await.result(f, scala.concurrent.duration.Duration.Inf)
  }
}
