package me.sgrouples.rogue.cc

import com.mongodb.MongoException
import com.mongodb.async.client.{MongoClient, MongoCollection, MongoDatabase}
import org.bson.BsonDocument
import org.bson.codecs.{IntegerCodec, LongCodec}
import org.bson.codecs.configuration.CodecRegistries

/**
  * Replacement of Mongo/MongoAsync from Lift
  */
object CcMongo {

  val codecRegistry = CodecRegistries.fromRegistries(com.mongodb.MongoClient.getDefaultCodecRegistry(),
    CodecRegistries.fromCodecs(new LongPrimitiveCodec, new IntegerPrimitiveCodec)
  )


  private[this] val bsonDocument = classOf[BsonDocument]
  private[this] val dbs = new scala.collection.concurrent.TrieMap[String, (MongoClient, String)]

  /**
    * Define a Mongo db using a MongoClient instance.
    */
  def defineDb(connId: String, mngo: MongoClient, dbName: String) {
    dbs.put(connId, (mngo, dbName))
  }

  /**
  * Get a DB reference
  */
  def getDb(connId: String): Option[MongoDatabase] = dbs.get(connId).map{case(client, dbName) => client.getDatabase(dbName).withCodecRegistry(codecRegistry)}

  def useDB[T](ci: String)(f: (MongoDatabase) => T): T = {
    val db = getDb(ci) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+ci.toString)
    }
    f(db)
  }

  /**
    * Executes function {@code f} with the mongo named {@code name} and collection names {@code collectionName}.
    * Gets a collection for you.
    */
  def useCollection[T](name: String, collectionName: String)(f: (MongoCollection[BsonDocument]) => T): T = {
    val coll = getCollection(name, collectionName) match {
      case Some(collection) => collection
      case _ => throw new MongoException("Mongo not found: "+collectionName+". ConnectionIdentifier: "+name.toString)
    }

    f(coll)
  }

  /*
* Get a Mongo collection. Gets a Mongo db first.
*/
  private def getCollection(name: String, collectionName: String): Option[MongoCollection[BsonDocument]] = getDb(name) match {
    case Some(mongo) if mongo != null => Some(mongo.getCollection(collectionName, bsonDocument))
    case _ => None
  }

  def closeAll(): Unit = {
    import scala.collection.JavaConversions._
    dbs.values.foreach { case (mngo, _) =>
      mngo.close()
    }
    dbs.clear()
  }

}

class LongPrimitiveCodec extends LongCodec {
  override def getEncoderClass() = java.lang.Long.TYPE
}

class IntegerPrimitiveCodec extends IntegerCodec {
  override def getEncoderClass() = java.lang.Integer.TYPE
}


