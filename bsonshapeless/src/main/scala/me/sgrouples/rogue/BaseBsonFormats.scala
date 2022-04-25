package me.sgrouples.rogue
import org.bson.*
import org.bson.types.{Decimal128, ObjectId}
import java.nio.{ByteBuffer, ByteOrder}
import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.util.{Currency, Locale, TimeZone, UUID}


trait BaseBsonFormats:
  val objectIdZero = new ObjectId(Array.fill[Byte](12)(0))

  given BooleanBsonFormat: BasicBsonFormat[Boolean] with
    def read(b: BsonValue): Boolean = b.asBoolean().getValue()
    def write(t: Boolean): BsonValue = new BsonBoolean(t)
    def defaultValue: Boolean = false


  given IntBsonFormat: BasicBsonFormat[Int] with
    override def read(b: BsonValue): Int = b.asNumber().intValue()
    override def write(t: Int): BsonValue = new BsonInt32(t)
    override def defaultValue: Int = 0


  given LongBsonFormat: BasicBsonFormat[Long] with
    override def read(b: BsonValue): Long = b.asNumber().longValue()
    override def write(t: Long): BsonValue = new BsonInt64(t)
    override def defaultValue: Long = 0L

  given StringBsonFormat: BasicBsonFormat[String] with
    override def read(b: BsonValue): String = b.asString().getValue()
    override def write(t: String): BsonValue = new BsonString(t)
    override def defaultValue: String = ""

  given ObjectIdBsonFormat: BasicBsonFormat[ObjectId] with
    override def read(b: BsonValue): ObjectId = b.asObjectId().getValue()
    override def write(t: ObjectId): BsonValue = new BsonObjectId(t)
    override def defaultValue: ObjectId = objectIdZero

  given DoubleBsonFormat: BasicBsonFormat[Double] with
    override def read(b: BsonValue): Double = b.asDouble().doubleValue()
    override def write(t: Double): BsonValue = new BsonDouble(t)
    override def defaultValue: Double = 0.0d

  given BigDecimalBsonFormat: BasicBsonFormat[BigDecimal] with
    override def read(b: BsonValue): BigDecimal =
      b.asDecimal128().decimal128Value().bigDecimalValue()
    override def write(t: BigDecimal): BsonValue = new BsonDecimal128(
      new Decimal128(t.bigDecimal)
    )
    override def defaultValue: BigDecimal = BigDecimal(0)

  given CurrencyBsonFormat: BasicBsonFormat[Currency] with
    override def read(b: BsonValue): Currency =
      Currency.getInstance(b.asString().getValue)
    override def write(t: Currency): BsonValue = new BsonString(
      t.getCurrencyCode
    )
    override def defaultValue: Currency = Currency.getInstance("USD")

  given LocaleBsonFormat: BasicBsonFormat[Locale] with
    override def read(b: BsonValue): Locale =
      val value = b.asString().getValue
      if (value.isEmpty) defaultValue
      else {
        if ("en" == value) Locale.ENGLISH
        else SupportedLocales.map.getOrElse(value, defaultValue)
      }
    override def write(l: Locale): BsonValue =
      new BsonString(l.toString)
    override def defaultValue: Locale = Locale.ENGLISH


  /** care must be taken - because of different UUID formats. by default codec
  * can read any type, but for write - decision must be made either format 3 -
  * JAVA_LEGACY or format 4 - UUID_STANDARD recommendation is to use
  * UUID_STANDARD, but java driver uses JAVA_LEGACY by default
  * @see
  *   [https://jira.mongodb.org/browse/JAVA-403] for explanation
  * @see
  *   [[org.bson.codecs.UuidCodec]] this implementation does not support
  *   C_SHARP_LEGACY codec at all
  */
  given UUIDBsonFormat: BasicBsonFormat[UUID] with
    override def read(bv: BsonValue): UUID =
      val b = bv.asBinary()
      val bytes = b.getData
      val bb = ByteBuffer.wrap(b.getData)
      if (b.getType == BsonBinarySubType.UUID_LEGACY.getValue)
        bb.order(ByteOrder.LITTLE_ENDIAN)
      else
        bb.order(ByteOrder.BIG_ENDIAN)
      new UUID(bb.getLong(0), bb.getLong(8))
    override def write(t: UUID): BsonValue =
      val bytes = ByteBuffer.allocate(16)
      bytes.order(ByteOrder.LITTLE_ENDIAN)
      bytes.putLong(0, t.getMostSignificantBits)
      bytes.putLong(8, t.getLeastSignificantBits)
      new BsonBinary(BsonBinarySubType.UUID_LEGACY, bytes.array())
    override def defaultValue: UUID = new UUID(0, 0)

  given LocalDateTimeBsonFormat: BasicBsonFormat[LocalDateTime] with
    override def read(b: BsonValue): LocalDateTime = LocalDateTime.ofInstant(
      Instant.ofEpochMilli(b.asDateTime().getValue),
      ZoneOffset.UTC
    )
    override def write(t: LocalDateTime): BsonValue = new BsonDateTime(
      t.toInstant(ZoneOffset.UTC).toEpochMilli
    )
    override def defaultValue: LocalDateTime =
      LocalDateTime.ofEpochSecond(0, 0, ZoneOffset.UTC)


  given InstantBsonFormat: BasicBsonFormat[Instant] with
    override def read(b: BsonValue): Instant =
    Instant.ofEpochMilli(b.asDateTime().getValue)
    override def write(t: Instant): BsonValue = new BsonDateTime(t.toEpochMilli)
    override def defaultValue: Instant = Instant.ofEpochMilli(0)

  given TimeZoneFormat: BasicBsonFormat[TimeZone] with
    override def read(b: BsonValue): TimeZone =
    TimeZone.getTimeZone(b.asString().getValue)
    override def write(tz: TimeZone): BsonValue = new BsonString(tz.getID)
    override def defaultValue: TimeZone = TimeZone.getTimeZone("UTC")

  given BinaryBsonFormat: BasicBsonFormat[Array[Byte]] with
    override def read(b: BsonValue): Array[Byte] = b.asBinary().getData
    override def write(t: Array[Byte]): BsonValue = new BsonBinary(t)
    override def defaultValue: Array[Byte] = Array.empty[Byte]

  /*protected def `@@AnyBsonFormat`[T, Tag](implicit
      tb: BsonFormat[T]
  ): BasicBsonFormat[T @@ Tag] = {
    new BasicBsonFormat[T @@ Tag] {
      override def read(b: BsonValue): T @@ Tag = tag[Tag][T](tb.read(b))
      override def write(t: T @@ Tag): BsonValue = tb.write(t)
      override def defaultValue: T @@ Tag = tag[Tag][T](tb.defaultValue)
    }
  }*/

  //  // TODO: this should be split to BsonReader[+T] and BsonWriter[-T] so that we wouldn't have to code all that by hand

  /*
  implicit def `@@StringBsonFormat`[Tag]: BsonFormat[String @@ Tag] =
    `@@AnyBsonFormat`[String, Tag]
  implicit def `@@LongBsonFormat`[Tag]: BsonFormat[Long @@ Tag] =
    `@@AnyBsonFormat`[Long, Tag]
  implicit def `@@IntBsonFormat`[Tag]: BsonFormat[Int @@ Tag] =
    `@@AnyBsonFormat`[Int, Tag]
  implicit def `@@DoubleBsonFormat`[Tag]: BsonFormat[Double @@ Tag] =
    `@@AnyBsonFormat`[Double, Tag]
  implicit def `@@ObjectIdBsonFormat`[Tag]: BsonFormat[ObjectId @@ Tag] =
    `@@AnyBsonFormat`[ObjectId, Tag]
  */



//given BasicBsonFormat[Int] with
