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
  def gen[T: Type](using Quotes): Expr[MacroBsonFormat[T]] =
    import quotes.reflect.*
    Expr.summon[MacroBsonFormat[T]] match {
      case Some(expr: Expr[_]) => expr
      case None => genDerived[T]
    }

  def genDerived[T: Type](using Quotes): Expr[MacroBsonFormat[T]] =
    import quotes.reflect.*
    //no mirror for traits -- need to work on types
    val ev: Option[Expr[Mirror.Of[T]]] = Expr.summon[Mirror.Of[T]]
    ev match
      case Some('{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes}}) =>
         //val all = summonAllMShow[elementTypes]
         //val elemInstances = summonAllFormats[elementTypes]
         genProduct[T]
      case Some('{ $m: Mirror.SumOf[T] { type MirroredElemTypes = elementTypes }}) =>
        genSum
      case Some(_) => 
        report.errorAndAbort(s"Strange mirror ${ev}")
      case None =>
        genNoMirror

  //defaults?
  def genProduct[T: Type](using Quotes):Expr[MacroBsonFormat[T]] =
    import quotes.reflect.*
    val tpe:TypeRepr = TypeRepr.of[T]
    val fields: List[Symbol] = TypeTree.of[T].symbol.caseFields 
    val fieldsVec = Expr.apply(fields.map(_.name))
    //TODO - why it can't be reused ?
    /*val termsTypes: List[(Symbol, (Term, TypeRepr))] = fields.map { field => 
        val fieldValDef: ValDef = field.tree.asInstanceOf[ValDef]  // TODO remove cast
        val fieldTpe: TypeRepr = fieldValDef.tpt.tpe
        field -> lookupFormatFor(fieldTpe)
    }*/ 
    
    def appendValsImpl(writer:Expr[_root_.org.bson.BsonWriter], t:Expr[T]): Expr[Unit] = {
      val bsonType = TypeRepr.of[_root_.org.bson.BsonWriter]
      val strType = TypeRepr.of[String]
      val vt = t.asTerm
      val fff = fields.map{ fld =>
        val fna = Expr(fld.name)
        val fieldValDef: ValDef = fld.tree.asInstanceOf[ValDef]  // TODO remove cast
        val fieldTpe: TypeRepr = fieldValDef.tpt.tpe
        val (wTerm, wType) = lookupFormatFor(fieldTpe)
        val fldVal = Select(t.asTerm, fld)
        val app = Select.unique(wTerm,"appendKV")
        report.info(app.show(using Printer.TreeShortCode))
        val append = Apply(app, List(writer.asTerm, fna.asTerm, fldVal))
        //println(append.show(using Printer.TreeShortCode))
        //report.warning(bl.show(using Printer.TreeStructure))
        append.asExpr
      }
      Expr.block(fff, '{()})
    }

    def writeValImpl(d:Expr[org.bson.BsonDocument],t:Expr[T]): Expr[Unit] = {
      //val fields: List[Symbol] = TypeTree.of[T].symbol.caseFields 
      val vt = t.asTerm
      val fff = fields.map{ fld =>
        val fna = Expr(fld.name)
        val fieldValDef: ValDef = fld.tree.asInstanceOf[ValDef]  // TODO remove cast
        val fieldTpe: TypeRepr = fieldValDef.tpt.tpe
        val (wTerm, wType) = lookupFormatFor(fieldTpe)
        val fldVal = Select(t.asTerm, fld)
        val wrExpr = Select.unique(wTerm, "write")
        val app = Apply(wrExpr, List(fldVal))
        '{ ${d}.append(${fna}, ${app.asExprOf[BsonValue]} ) }
      }
      Expr.block(fff, '{()})
    }

    def readValImpl(b:Expr[_root_.org.bson.BsonDocument]): Expr[T] = {
      val fieldVals = fields.map{ fld =>
        val fna = Expr(fld.name)
        val fieldValDef: ValDef = fld.tree.asInstanceOf[ValDef]  // TODO remove cast
        val fieldTpe: TypeRepr = fieldValDef.tpt.tpe
        val (wTerm, wType) = lookupFormatFor(fieldTpe)
        val rExpr = Select.unique(wTerm,"readOrDefault")
        val fldVal = '{ ${b}.get(${fna}) }
        val readV = Apply(rExpr, List(fldVal.asTerm))
        readV
      }
      val cs = tpe.classSymbol.get
      val app = Apply(Select(New(TypeTree.of[T]), cs.primaryConstructor), fieldVals).asExprOf[T]
      app
    }
    

    val r = '{
      new MacroBaseBsonFormat[T] {
        //${ b.asExpr }
        //${vd.asExpr}
        //${ vd.asExpr }
        //${fields.map { s => Expr(s = 1)} }
        //${Expr.apply(h) = 1 } 
        //..$bsonFormats
       // override val flds = ${ Expr(fldsMap) } //Map(..$fldsMap) //++ Seq(..$subFieldsAdd).flatten
        override def validNames():Vector[String] = ${fieldsVec}.toVector //why no vector?

        override def defaultValue: T = {
          //super ugly hack, but whatever
          read (new org.bson.BsonDocument())
        }

        override def read(b: _root_.org.bson.BsonValue): T = {
          val doc = if(b.isDocument()) {
            b.asDocument()
          } else {
            new org.bson.BsonDocument()
          }
          ${readValImpl('doc)}
        }

        override def write(t: T): _root_.org.bson.BsonValue =
          val docx = new _root_.org.bson.BsonDocument()
          val writer = new org.bson.BsonDocumentWriter(docx)
          //${writeValImpl('docx, 't)}
          append(writer, t)
          writer.close()
          docx
        
        private def appendVals(writer:_root_.org.bson.BsonWriter, t:T): Unit =
          ${appendValsImpl('writer, 't)}
          //{}
      
        override def append(writer:_root_.org.bson.BsonWriter, k:String, t:T): Unit = {
          writer.writeStartDocument(k)
          appendVals(writer, t)
          writer.writeEndDocument()
        }
        override def append(writer:_root_.org.bson.BsonWriter, t:T): Unit = {
          writer.writeStartDocument()
          appendVals(writer, t)
          writer.writeEndDocument()
        }
     }
    }
    println(r.asTerm.show(using Printer.TreeShortCode))
    r
   

  def genSum[T: Type](using  Quotes):Expr[MacroBaseBsonFormat[T]] =
    import quotes.reflect.*
    report.errorAndAbort(s"SUM type not yet supported ")
    

  /**
    * For given type t, return Term and TypeRepr of BsonFormat[t]
    *
    * @param quotes
    * @param t
    * @return 
    */
  def lookupFormatFor(using quotes: Quotes)(t: quotes.reflect.TypeRepr): (quotes.reflect.Term, quotes.reflect.TypeRepr) =
    import quotes.reflect.*
    val showTpe = TypeRepr.of[MacroBsonFormat]
    val tclTpe: TypeRepr = showTpe.appliedTo(t)
    Implicits.search(tclTpe) match
      case res: ImplicitSearchSuccess => (res.tree, tclTpe)
      case _ => report.errorAndAbort(s"No Show format for ${tclTpe.typeSymbol}")

    /*val elemInstances = summonAll[elementTypes]
    val eqSumBody: (Expr[T], Expr[T]) => Expr[Boolean] = (x, y) =>
      val ordx = '{ $m.ordinal($x) }
      val ordy = '{ $m.ordinal($y) }
      val elements = Expr.ofList(elemInstances)
      '{ $ordx == $ordy && $elements($ordx).asInstanceOf[Eq[Any]].eqv($x, $y) }
    '{ eqSum((x: T, y: T) => ${eqSumBody('x, 'y)}) }
    println(s"IN given - ${ev}")
    //val A = TypeRepr.of[A]
    '{new MShow[T] {
      def show(t: T): String = "a.b.c"
    }
    }*/

  /*
  def anns(s: Symbol): Expr[List[Annotation]] =
    Expr.ofList(s.annotations.map(_.asExpr).collect {
      case '{ $a: Annotation } => a
    }.filter {
      case '{ $a: internal.Child[_] } => false
      case _ => true
    }
  )
  */

  def genNoMirror[T: Type](using Quotes):Expr[MacroBaseBsonFormat[T]] =
    import quotes.reflect.*
    report.errorAndAbort(s"No derivation possible without mirror")
    /*
    val treeType = TypeTree.of[T]
    val tpeRepr = treeType.tpe
    val r = tpeRepr.show(using Printer.TypeReprStructure)
    tpeRepr match
      case AppliedType(base, args) =>
        val m = Map(1->2)
        val isArray = base.baseClasses.contains(Symbol.classSymbol("scala.Array"))
        val isIterable = base.baseClasses.contains(Symbol.classSymbol("scala.collection.Iterable"))
        val isMap = base.baseClasses.contains(Symbol.classSymbol("scala.collection.immutable.Map"))
        val b = base.show(using Printer.TypeReprShortCode)
        report.info(s"base is $b \nisArray $isArray\nisIterable $isIterable\nisMap $isMap")
        '{
          new MacroBaseBsonFormat[T]:
            def mshow(t:T) = ${Expr(r)} + " genNoMirror "
         }
      case t: NamedType =>
        report.errorAndAbort(s"NamedType not supported for $r")
      case o: OrType =>
        report.errorAndAbort(s"OrType not supported for $r")
      case AndType(base, AppliedType(x@TypeRef(_, "Tagged"), y)) => // cheap hack for tagged Types 
        val tagType = y.last.asType
        val baseType = base.asType */
        // only for tagged types
        //val left = l.show(using Printer.TypeReprStructure)
        //a.left.asType
        //val tl = l.asType
        //val right = y.head.show(using Printer.TypeReprStructure)
        //val all = tpeRepr.show(using Printer.TypeReprStructure)
        //report.errorAndAbort(s"left ${left}\nright ${right}\nall ${all}") 
      
        // report.info(s"e1 ${e1.show(using Printer.)}")
        //val (baseMShowTerm, baseMShowType) = lookupShowFor(base) //.asExprOf[MShow[l]]
        //val baseMType = baseMShowType.asType
        //val baseShowExpr = baseMShowTerm.asExprOf[MShow[baseType.Underlying]] //.asExprOf[baseMType.Underlying].
        //val lf = showForAnd.show(using Printer.TreeShortCode) 
        //report.info(s"for left found $lf")
        //val wr = TypeTree.of[WrappedMShow]
        //TypeApply(wr.tpe, List(tpeRepr))
        /*Apply(Select.apply())
        '{ 
          WrappedMShow[T, baseType.Underlying]($baseShowExpr)
        }*/
        //report.info(s"AndType only for\n   left $left   right $right")
        //report.error(s"AndType not supported for $r") 
      //case a@AndType(l,r) =>
        //report.errorAndAbort(s"Generic AndType not supported ${tpeRepr}")
    //option
    //array
    //map
    //iterable 
    //tpeRepr match 
    //  case AppliedType()
     // val m = MapTypeObject 
    //val iterableTypeSymbol = typeOf[Iterable[_]].typeSymbol
    //val at = appliedType(tpeRepr, tpeRepr.a)
    //f at.baseClasses.contains(iterableTypeSymbol)
    //if tpeRepr.termSymbol
    //val x = Expr[String](tpeRepr.typeSymbol.fullName)
    //val x = Expr[String](treeType.symbol.fullName)
    //report.info(s"x is ${x}")

  
