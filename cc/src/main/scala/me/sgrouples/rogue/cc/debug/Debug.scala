package me.sgrouples.rogue.cc.debug

import scala.collection.{ Iterable, mutable }
import scala.language.higherKinds

trait Debug[T] {

  def show(t: T): String

}

object Debug {

  def debug[T: Debug](t: T): String = implicitly[Debug[T]].show(t)

  object DefaultImplicits {

    implicit object DebugString extends Debug[String] {
      override def show(t: String): String = t
    }

    implicit object DebugInt extends Debug[Int] {
      override def show(t: Int): String = t.toString
    }

    implicit def debugTuple[A: Debug, B: Debug] = new Debug[(A, B)] {
      override def show(t: (A, B)): String =
        s"${implicitly[Debug[A]].show(t._1)}: ${implicitly[Debug[B]].show(t._2)}"
    }

    implicit def debugTraversable[T, W[_]](
      implicit
      sh: Debug[T],
      ev: W[T] <:< Iterable[T]): Debug[W[T]] = {
      new Debug[W[T]] {
        override def show(t: W[T]): String = {
          val buffer: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty
          for (x <- t) buffer += implicitly[Debug[T]].show(x)
          buffer.zipWithIndex.map(_.swap).map(Debug.debug(_)).mkString("\n")
        }
      }
    }
  }
}

