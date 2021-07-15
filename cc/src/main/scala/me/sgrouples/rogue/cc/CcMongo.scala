package me.sgrouples.rogue.cc


import io.fsq.rogue.codecs.{IntegerPrimitiveCodec, LongPrimitiveCodec}
import org.bson.codecs.configuration.CodecRegistries
import org.bson.types.ObjectId
import org.mongodb.scala.MongoClient

import java.time.Instant

object CcMongo {

  def oidFromInstant(d:Instant): ObjectId = {
    val timestamp =d.getEpochSecond()
    val bytes = new Array[Byte](12)
    bytes(0) = (timestamp >> 24).toByte
    bytes(1) = (timestamp >> 16).toByte
    bytes(2) = (timestamp >> 8).toByte
    bytes(3) = timestamp.toByte
    new ObjectId(bytes)
  }

  val codecRegistry = CodecRegistries.fromRegistries(
    MongoClient.DEFAULT_CODEC_REGISTRY,
    CodecRegistries.fromCodecs(new LongPrimitiveCodec, new IntegerPrimitiveCodec))
}