package me.sgrouples.rogue.cc.macros
import scala.quoted.*
import scala.compiletime.*
import scala.deriving.Mirror
import scala.annotation.meta.field
import me.sgrouples.rogue.BsonFormat
import scala.annotation.experimental
import org.bson.BsonWriter
import org.mongodb.scala.bson.BsonDocument.apply
import org.bson.BsonValue

object MacroBsonFormatDerivingImpl:
  private val debug: Boolean = Option(System.getenv("ROGUE_MACRO_DEBUG"))
    .flatMap(_.toBooleanOption)
    .contains(true)

  def genAuto[T: Type](using Quotes): Expr[MacroBsonFormat[T]] =
    import quotes.reflect.*
    Expr.summon[MacroBsonFormat[T]] match {
      case Some(expr: Expr[_]) => expr
      case None                => genDerived[T]
    }

  def gen[T: Type](using Quotes): Expr[MacroBsonFormat[T]] =
    import quotes.reflect.*
    genDerived[T]

  def genDerived[T: Type](using Quotes): Expr[MacroBsonFormat[T]] =
    import quotes.reflect.*
    //no mirror for traits -- need to work on types
    val ev: Option[Expr[Mirror.Of[T]]] = Expr.summon[Mirror.Of[T]]
    ev match
      case Some('{
            $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes }
          }) =>
        genProduct[T]
      case Some('{
            $m: Mirror.SumOf[T] { type MirroredElemTypes = elementTypes }
          }) =>
        genSum
      case Some(_) =>
        report.errorAndAbort(s"Strange mirror ${ev}")
      case None =>
        genNoMirror

  //defaults?
  def genProduct[T: Type](using Quotes): Expr[MacroBsonFormat[T]] =
    import quotes.reflect.*
    val tpe: TypeRepr = TypeRepr.of[T]
    val fields: List[Symbol] = TypeTree.of[T].symbol.caseFields
    val fieldsVec = Expr.apply(fields.map(_.name))
    val fieldsWithFormats
        : List[(Symbol, Expr[String], Expr[MacroBsonFormat[?]])] =
      fields.map { fld =>
        val fieldValDef: ValDef =
          fld.tree.asInstanceOf[ValDef] // TODO remove cast
        val fieldTpe: TypeRepr = fieldValDef.tpt.tpe
        val (wTerm, wType) = lookupFormatFor(fieldTpe)
        (fld, Expr(fld.name), wTerm.asExprOf[MacroBsonFormat[?]])
      }

    def appendValsImpl(
        fieldsWithFormats: List[(Symbol, Expr[String], Ref)],
        writer: Expr[_root_.org.bson.BsonWriter],
        t: Expr[T]
    ): Expr[Unit] = {
      def writerAppendField(
          writer: Expr[_root_.org.bson.BsonWriter],
          t: Expr[T],
          fieldSymbol: Symbol,
          fieldName: Expr[String],
          formatRef: Ref
      ): Expr[Any] = {
        val fldVal = Select(t.asTerm, fieldSymbol)
        val app = Select.unique(formatRef, "appendKV")
        val append = Apply(app, List(writer.asTerm, fieldName.asTerm, fldVal))
        append.asExpr
      }

      val bsonType = TypeRepr.of[_root_.org.bson.BsonWriter]
      val strType = TypeRepr.of[String]
      val vt = t.asTerm
      val fff = fieldsWithFormats.map { case (symbol, name, format) =>
        writerAppendField(writer, t, symbol, name, format)
      }
      Expr.block(fff, '{ () })
    }

    def writeImpl(
        fieldsWithFormats: List[(Symbol, Expr[String], Ref)],
        d: Expr[org.bson.BsonDocument],
        t: Expr[T]
    ): Expr[Unit] = {
      def writeField(
          d: Expr[org.bson.BsonDocument],
          t: Expr[T],
          fieldSymbol: Symbol,
          fieldName: Expr[String],
          formatRef: Ref
      ) = {
        val fldVal = Select(t.asTerm, fieldSymbol)
        val wrExpr = Select.unique(formatRef, "write")
        val app = Apply(wrExpr, List(fldVal))
        val bsonVal = app.asExprOf[BsonValue]
        ValDef
          .let(Symbol.spliceOwner, "bson", bsonVal.asTerm) { ref =>
            '{
              if (!${ ref.asExprOf[BsonValue] }.isNull()) {
                ${ d }.append(${ fieldName }, ${ ref.asExprOf[BsonValue] })
              }
            }.asTerm
          }
          .asExpr
      }

      val vt = t.asTerm
      val fff = fieldsWithFormats.map { case (symbol, name, format) =>
        writeField(d, t, symbol, name, format)
      }
      Expr.block(fff, '{ () })
    }

    def readImpl(
        fieldsWithFormats: List[(Symbol, Expr[String], Ref)],
        b: Expr[_root_.org.bson.BsonDocument]
    ): Expr[T] = {
      def readField(
          b: Expr[_root_.org.bson.BsonDocument],
          fieldName: Expr[String],
          formatRef: Ref
      ) = {
        val rExpr = Select.unique(formatRef, "readOrDefault")
        val fldVal = '{ ${ b }.get(${ fieldName }) }
        Apply(rExpr, List(fldVal.asTerm))
      }

      val fieldVals = fieldsWithFormats.map { case (_, name, format) =>
        readField(b, name, format)
      }
      val cs = tpe.classSymbol.get
      val app =
        Apply(Select(New(TypeTree.of[T]), cs.primaryConstructor), fieldVals)
          .asExprOf[T]
      app
    }

    def fldsFormatsMap: Expr[Map[String, MacroBsonFormat[?]]] = {
      val (ek, ev) = fieldsWithFormats.map { case (_, name, format) =>
        (name, format)
      }.unzip
      val keyExprs = Expr.ofList[String](ek)
      val valExprs = Expr.ofList[MacroBsonFormat[?]](ev)
      val res = '{ ($keyExprs.zip($valExprs)).toMap }
      res
    }

    val r = ValDef
      .let(Symbol.spliceOwner, fieldsWithFormats.map(_._3.asTerm)) { refs =>
        val fieldsWithFormatRefs = fieldsWithFormats.zip(refs).map {
          case ((fieldSymbol, fieldName, _), formatRef) =>
            (fieldSymbol, fieldName, formatRef)
        }

        '{
          new MacroBaseBsonFormat[T] {
            override def validNames(): Vector[String] = ${
              fieldsVec
            }.toVector //why no vector?

            override val flds: Map[String, BsonFormat[?]] =
              ${ fldsFormatsMap } ++ (${ fldsFormatsMap }
                .map { case (name, f) => subfields(name, f) })
                .flatten
                .toMap[String, BsonFormat[?]]

            override def defaultValue: T = {
              //super ugly hack, but whatever
              read(new org.bson.BsonDocument())
            }

            override def read(b: _root_.org.bson.BsonValue): T = {
              val doc = if (b.isDocument()) {
                b.asDocument()
              } else {
                new org.bson.BsonDocument()
              }
              ${ readImpl(fieldsWithFormatRefs, 'doc) }
            }

            override def write(t: T): _root_.org.bson.BsonValue =
              val docx = new _root_.org.bson.BsonDocument()
              ${ writeImpl(fieldsWithFormatRefs, 'docx, 't) }
              docx

            private def appendVals(
                writer: _root_.org.bson.BsonWriter,
                t: T
            ): Unit =
              ${ appendValsImpl(fieldsWithFormatRefs, 'writer, 't) }

            override def append(
                writer: _root_.org.bson.BsonWriter,
                k: String,
                t: T
            ): Unit = {
              writer.writeStartDocument(k)
              appendVals(writer, t)
              writer.writeEndDocument()
            }

            override def append(
                writer: _root_.org.bson.BsonWriter,
                t: T
            ): Unit = {
              writer.writeStartDocument()
              appendVals(writer, t)
              writer.writeEndDocument()
            }
          }
        }.asTerm
      }
      .asExpr
      .asInstanceOf[Expr[MacroBsonFormat[T]]]

    if debug then report.info(r.show)

    r

  def genSum[T: Type](using Quotes): Expr[MacroBaseBsonFormat[T]] =
    import quotes.reflect.*
    report.errorAndAbort(s"SUM type not yet supported ")

  /** For given type t, return Term and TypeRepr of BsonFormat[t]
    *
    * @param quotes
    *   @param t
    * @return
    */
  def lookupFormatFor(using quotes: Quotes)(
      t: quotes.reflect.TypeRepr
  ): (quotes.reflect.Term, quotes.reflect.TypeRepr) =
    import quotes.reflect.*
    val showTpe = TypeRepr.of[MacroBsonFormat]
    val tclTpe: TypeRepr = showTpe.appliedTo(t)
    Implicits.search(tclTpe) match
      case res: ImplicitSearchSuccess => (res.tree, tclTpe)
      case fail =>
        report.errorAndAbort(s"No MacroBsonFormat format for ${t.show}")

  def genNoMirror[T: Type](using Quotes): Expr[MacroBaseBsonFormat[T]] =
    import quotes.reflect.*
    report.errorAndAbort(s"No derivation possible without mirror")
