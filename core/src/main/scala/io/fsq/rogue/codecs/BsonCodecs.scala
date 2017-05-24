package io.fsq.rogue.codecs

import org.bson.codecs.{ IntegerCodec, LongCodec }

class LongPrimitiveCodec extends LongCodec {
  override def getEncoderClass() = java.lang.Long.TYPE
}

class IntegerPrimitiveCodec extends IntegerCodec {
  override def getEncoderClass() = java.lang.Integer.TYPE
}

