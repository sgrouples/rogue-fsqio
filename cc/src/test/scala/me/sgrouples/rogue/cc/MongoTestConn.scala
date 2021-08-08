package me.sgrouples.rogue.cc

import org.mongodb.scala._

object MongoTestConn {

  var client: Option[MongoClient] = None

  def connectToMongo(): MongoClient = {
    val (host, port) = Option(System.getProperty("default.mongodb.server"))
      .map({ str =>
        val arr = str.split(':')
        (arr(0), arr(1).toInt)
      })
      .getOrElse(("localhost", 51101))
    val cl = MongoClient(s"mongodb://${host}:${port}")
    client = Option(cl)
    cl
  }

  def disconnectFromMongo() = {
    client.foreach(_.close())
    client = None
  }

}
