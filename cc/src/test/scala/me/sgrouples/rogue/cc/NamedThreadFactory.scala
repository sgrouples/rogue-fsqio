package me.sgrouples.rogue.cc

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

class NamedThreadFactory(delegate: ThreadFactory, namePrefix: String) extends ThreadFactory {
  private val threadCount: AtomicInteger = new AtomicInteger (0)
  override def newThread (runnable: Runnable): Thread = {
  val thread: Thread = delegate.newThread (runnable)
  thread.setName (namePrefix + "-" + threadCount.getAndIncrement)
  thread
}
}
