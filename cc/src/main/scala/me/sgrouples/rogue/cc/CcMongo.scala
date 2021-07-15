package me.sgrouples.rogue.cc


import io.fsq.rogue.codecs.{IntegerPrimitiveCodec, LongPrimitiveCodec}
import org.bson.codecs.configuration.CodecRegistries
import org.mongodb.scala.{MongoClient}

object CcMongo {

  val codecRegistry = CodecRegistries.fromRegistries(
    MongoClient.DEFAULT_CODEC_REGISTRY,
    CodecRegistries.fromCodecs(new LongPrimitiveCodec, new IntegerPrimitiveCodec))
}