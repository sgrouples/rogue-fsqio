package me.sgrouples.rogue.cc

import java.util.concurrent.atomic.AtomicInteger

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import shapeless.tag
import shapeless.tag._

import scala.collection.mutable

trait MacroNamesResolver[T] extends NamesResolver {
  protected var resolved: Boolean = false
  private[this] val nameId = new AtomicInteger(-1)
  protected def nextNameId = nameId.incrementAndGet()
  private[this] def resolveError(id: Int): Nothing = throw new IllegalStateException(debugInfo(id))
  private[cc] def debugInfo(id: Int): String = {
    s"error macro resolving id ${id}"
  }
  private[this] val fields: mutable.Map[String, io.fsq.field.Field[_, _]] = mutable.Map.empty
  protected val names: mutable.Map[Int, String] = mutable.Map.empty[Int, String]
  def resolve(): Unit = {
    println("default resolve called")
  }

  override def named[T <: io.fsq.field.Field[_, _]](name: String)(func: String => T): T @@ Marker = {
    if (!resolved) resolve()
    names += nextNameId -> name
    val field = func(name)
    if (fields.contains(name)) throw new IllegalArgumentException(s"Field with name $name is already defined")
    fields += (name -> field)
    tag[Marker][T](field)
  }
  override def named[T <: io.fsq.field.Field[_, _]](func: String => T): T @@ Marker = {
    if (!resolved) resolve()
    val nextId = nextNameId

    val name = names.getOrElse(nextId, resolveError(nextId))

    val field = func(name)
    if (fields.contains(name)) throw new IllegalArgumentException(s"Field with name $name is already defined")
    fields += (name -> field)
    tag[Marker][T](field)
  }
}

object MacroCC {
  implicit def gen[T]: MacroNamesResolver[T] = macro MacroCCGenerator.genImpl[T]
}

class MacroCCGenerator(val c: Context) {
  import c.universe._
  def genImpl[T: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[T]
    val members = tpe.members
    val terms = members.flatMap { symbol =>
      if (symbol.isTerm) {
        val term = symbol.asTerm
        if (term.isAccessor && term.getter.isMethod) {
          Some(term.name)
        } else None
      } else None
    }
    val nameMap = terms.zipWithIndex.map {
      case (n, i) =>
        (i -> s"$n")
    }.toVector

    //println(s"terms ${terms}")
    val name = c.freshName()
    val r = q"""new MacroNamesResolver[$tpe] {
                override def resolve():Unit = {
                println("overriden resolve called")
       this.names ++= ${nameMap}
       this.resolved = true
       }
       def addedUnit:Unit = {
       println("Added unit")
       }
       println("Calling override resolve")
       resolve()
    }
    """
    println("sss")
    println(s"Will return ${r}")
    r
    //c.Expr[MacroNamesResolver[T]](r)
  }
}
