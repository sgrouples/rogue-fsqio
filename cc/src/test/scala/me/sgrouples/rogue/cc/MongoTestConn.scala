package me.sgrouples.rogue.cc

import com.mongodb.connection.StreamFactoryFactory
import com.mongodb.connection.netty.NettyStreamFactoryFactory
import org.bson.UuidRepresentation
import org.mongodb.scala.{MongoClientSettings, _}
import io.netty.channel.EventLoopGroup
import io.netty.channel.epoll.{Epoll, EpollEventLoopGroup, EpollSocketChannel}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel

import java.util.concurrent.Executors

object MongoTestConn {

  var client: Option[MongoClient] = None

  def create(threadNamePrefix: String): EventLoopGroup = {
    if (Epoll.isAvailable) {
      new EpollEventLoopGroup(1, new NamedThreadFactory(Executors.defaultThreadFactory(),"epoll-test-"))
    } else {
      new NioEventLoopGroup(1,  new NamedThreadFactory(Executors.defaultThreadFactory(),"nio-test"))
    }
  }

  def nettyStreamFactoryFactory(eventLoopGroup: EventLoopGroup): StreamFactoryFactory = {
    val socketChannelClass = if (Epoll.isAvailable) {
      classOf[EpollSocketChannel]
    } else {
      classOf[NioSocketChannel]
    }
    NettyStreamFactoryFactory.builder().eventLoopGroup(eventLoopGroup).socketChannelClass(socketChannelClass).build()
  }

  def connectToMongo(): MongoClient = {
    val (host, port) = Option(System.getProperty("default.mongodb.server"))
      .map({ str =>
        val arr = str.split(':')
        (arr(0), arr(1).toInt)
      })
      .getOrElse(("localhost", 51101))
    val sff = nettyStreamFactoryFactory(create("XX"))
    val settings = MongoClientSettings.builder()
      .applyConnectionString(ConnectionString(s"mongodb://${host}:${port}"))
      .streamFactoryFactory(sff)
      .uuidRepresentation(UuidRepresentation.JAVA_LEGACY)
      .build()
    val cl = MongoClient(settings)
    client = Option(cl)
    cl
  }

  def disconnectFromMongo(): Unit = {
    client.foreach(_.close())
    client = None
  }

}
