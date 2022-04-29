package me.sgrouples.rogue.cc.macros
import scala.quoted.*
import scala.compiletime.*
import scala.deriving.Mirror
import scala.annotation.meta.field
import me.sgrouples.rogue.BsonFormat

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
    //val flds = tpe.termSymbol.caseFields
    //val flds = tpe.typeSymbol.primaryConstructor.paramSymss.flatten
    val fields: List[Symbol] = TypeTree.of[T].symbol.caseFields 
    val fieldsVec = Expr.apply(fields.map(_.name))
    val termsTypes: List[(Symbol, (Term, TypeRepr))] = fields.map { field => 
        val fieldValDef: ValDef = field.tree.asInstanceOf[ValDef]  // TODO remove cast
        val fieldTpe: TypeRepr = fieldValDef.tpt.tpe
        field -> lookupFormatFor(fieldTpe)
    }
    /*termsTypes.foreach{ t => 
        report.info(s"term types $t")
    }*/

    //def accessors(expr: Expr[T]): List[Expr[Any]] = flds.map(Select(expr.asTerm, _).asExpr)
    //val f1 = flds.head
    //.map(_.name.trim)
    //val fieldAndShows: List[(Expr[MShow[_]], Symbol)] = mshows zip flds
    /*val ex = fieldNames.zip.show.map { case (fld, s) =>
      `Expr(fld)=s.show(t.fld)`
    }*/
    //shows.foreach(s=> println(s.asTerm.show(using Printer.TreeStructure)))
    //val y = fieldAndShows.map{ case(sh, f) => 't.${f} = $sh.show($t.f) }
    //val mshowBody: Expr[T] => Expr[String] => t => '{ "ALA" }
      
    //val mshowBody: Expr[T] => Expr[String] = t => 
    //      '{ $sh1.mshow(Select(t.asTerm, fh)) }

    /*def showField(caseClassTerm: Term, field: Symbol /*, showExpr:Expr[MShow[_]]*/): Expr[String] =
      val fieldValDef: ValDef = field.tree.asInstanceOf[ValDef]  // TODO remove cast
      val fieldTpe: TypeRepr = fieldValDef.tpt.tpe
      val fieldName: String = fieldValDef.name
      val (showTerm, showTpe) = lookupShowFor(fieldTpe) //showExpr.tree
      val fieldValue = Select(caseClassTerm, field) // v.field
      val strRepr = Apply(Select.unique(showTerm,"mshow"), List(fieldValue)).asExprOf[String]
      '{s"${${Expr(fieldName)}}: ${${strRepr}}"}  // summon[Show[$fieldTpe]].show(v.field)
    */
    //def showBody(v: Expr[T]): Expr[String] =
      //val vTerm: Term = v.asTerm
      //val valuesExprs: List[Expr[String]] = fields.map(showField(vTerm, _))
      //val exprOfList: Expr[List[String]] = Expr.ofList(valuesExprs)
      //'{$exprOfList.mkString(", ")}
    
    val fmtValsAndRefs = termsTypes.map { case ( fSym, (fTerm, tTypeRepr)) =>
        val v =  Symbol.newVal(
            Symbol.spliceOwner,
            fSym.name +"_fmt",
            tTypeRepr,
            Flags.EmptyFlags,
            Symbol.noSymbol
            )
        (ValDef(v, Some(fTerm)), Ref(v))
        }
    val (fmtVals, fmtRefs) = fmtValsAndRefs.unzip
    val formats = fmtRefs.map(_.asInstanceOf[BsonFormat[_]])

    val fldsMap =(fields.map(_.name) zip fmtRefs).map{ case (k, v) => k -> v.asInstanceOf[BsonFormat[_]] }.toMap
    
    
    val b = Block(fmtVals, fmtRefs.head) 

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
            ???
         // $defImpl
        }

        override def read(b: _root_.org.bson.BsonValue): T = {
          if(b.isDocument()) {
            val doc = b.asDocument()
            
            ???
            //new $tpe(..$reads)
          } else {
            defaultValue
          }
        }

        override def write(t: T): _root_.org.bson.BsonValue = {
          val doc = new _root_.org.bson.BsonDocument()
          //..$writers
          doc
        }
        
        private def appendVals(writer:_root_.org.bson.BsonWriter, t:T): Unit = {
        
          //..$appends
        }
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
    //r.asTerm.show(using Printer.TreeStructure)
    report.info(r.show)
    r

  def genSum[T: Type](using  Quotes):Expr[MacroBaseBsonFormat[T]] =
    import quotes.reflect.*
    report.errorAndAbort(s"SUM type not yet supported ")
    

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

  
