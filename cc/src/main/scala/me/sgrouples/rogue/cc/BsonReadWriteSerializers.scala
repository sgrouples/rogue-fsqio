package me.sgrouples.rogue.cc

import io.fsq.rogue.MongoHelpers.MongoSelect
import org.bson.{BsonArray, BsonDocument, BsonNull, BsonValue}

/**
 * Created by mar on 24.07.2016.
 */
trait BsonReadWriteSerializers[MB <: CcMeta[_]] extends ReadWriteSerializers[MB] {

  override protected def readSerializer[M <: CcMeta[_], R](meta: M, select: Option[MongoSelect[M, R]]): RogueBsonRead[R] = {
    new RogueBsonRead[R] {
      override def fromDocument(dbo: BsonDocument): R = select match {
        case Some(MongoSelect(fields, transformer, true, _)) if fields.isEmpty =>
          // A MongoSelect clause exists, but has empty fields. Return null.
          // This is used for .exists(), where we just want to check the number
          // of returned results is > 0.
          transformer(null)
        case Some(MongoSelect(fields, transformer, _, _)) =>
          //TODO - optimze - extract readers for fields up, before using read serializer. this is super -ineffective
          val values =
            fields.map(fld => {
              val (bsonV, readArray) = readBsonVal(dbo, fld.field.name)
              val reader = meta.reader(fld.field)
              //TODO - does not in case reader is for non-array, and subselect returns array
              //if fld is optional, readOpt will read Option[Option[T]] this is handled (poorly) inside fld.valueOrDefault
              fld.valueOrDefault(
                if (readArray) {
                  readOptArr(reader.readArray, bsonV.asArray())
                } else {
                  readOpt(reader.read, bsonV)
                })
            })
          transformer(values)

        case None =>
          //TODO - better types !!!!
          meta.read(dbo).asInstanceOf[R]
      }

      //same thing, but opt
      override def fromDocumentOpt(dbo: BsonDocument): Option[R] = select match {
        case Some(MongoSelect(fields, transformer, true, _)) if fields.isEmpty =>
          throw new RuntimeException("empty transformer for fromDocumentOpt not implemented, fields subset return in findAndModify not yet implemented")
        case Some(MongoSelect(fields, transformer, _, _)) =>
          throw new RuntimeException("fromDocumentOpt with fields subset return in findAndModify not yet implemented")
        case None => {
          //println(s"fromDocumentOpt: ${dbo} ")
          readOpt(meta.read, dbo).asInstanceOf[Option[R]]
        }
      }
    }
  }
  private[this] def readOpt[T](reader: BsonValue => T, v: BsonValue): Option[T] = {
    if (v == null || v.isNull) None
    else Option(reader(v))
  }

  private[this] def readOptArr[T](reader: BsonArray => Seq[T], v: BsonArray): Option[Seq[T]] = {
    if (v == null || v.isNull) None
    else Option(reader(v))
  }

  private[this] def readBsonVal(dbo: BsonDocument, fldName: String): (BsonValue, Boolean) = {
    val parts = fldName.split('.')
    //println(s"parts fldName ${fldName} len ${parts.length} ${parts}")
    var i = 0
    var needArray = false
    var d: BsonValue = dbo
    while (i < parts.length) {
      if (d == null) {
        i = parts.length
        d = BsonNull.VALUE
      } else {
        val key = parts(i)
        if (key == "$") {
          //TODO - probably it means that array is needed
          i = i + 1
        } else {
          if (d.isArray) {
            val r = new BsonArray()
            val it = d.asArray().iterator()
            while (it.hasNext) {
              r.add(it.next().asDocument().get(key))
            }
            d = r
            needArray = true
          } else {
            d = d.asDocument().get(key)
          }
          i = i + 1
        }
      }
    }
    (d, needArray)
  }

  override protected def writeSerializer[M <: CcMeta[_], R](meta: M): RogueBsonWrite[R] = {
    new RogueBsonWrite[R] {
      override def toDocument(record: R): BsonDocument = {
        meta.writeAnyRef(record.asInstanceOf[AnyRef]).asDocument()
      }
    }
  }
}
