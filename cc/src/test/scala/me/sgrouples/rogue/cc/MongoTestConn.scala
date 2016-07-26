package me.sgrouples.rogue.cc

import com.mongodb.async.client.{MongoClient, MongoClients}
import com.mongodb.{MongoClient => MongoClientSync}

object MongoTestConn {

  var client: Option[MongoClient] = None
  var clientSync: Option[MongoClientSync] = None

  def connectToMongo = {
    val (host, port) = Option(System.getProperty("default.mongodb.server")).map({ str =>
      val arr = str.split(':')
      (arr(0), arr(1).toInt)
    }).getOrElse(("localhost", 51101))
    val cl = MongoClients.create(s"mongodb://${host}:${port}")
    client = Option(cl)
    cl
  }

  def connectToMongoSync = {
    val (host, port) = Option(System.getProperty("default.mongodb.server")).map({ str =>
      val arr = str.split(':')
      (arr(0), arr(1).toInt)
    }).getOrElse(("localhost", 51101))
    val cl = new MongoClientSync(host, port)
    clientSync = Option(cl)
    cl
  }

  def disconnectFromMongo = {
    client.foreach(_.close())
    client = None
  }

  def disconnectFromMongoSync = {
    clientSync.foreach(_.close())
    clientSync = None
  }

}
