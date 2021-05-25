import java.util.concurrent.atomic.AtomicInteger

import de.flapdoodle.embed.mongo._
import de.flapdoodle.embed.mongo.Command
//import de.flapdoodle.embed.mongo.config.{MongoCmdOptionsBuilder, _}
import de.flapdoodle.embed.mongo.distribution.Version
import de.flapdoodle.embed.process.config.io.ProcessOutput
import de.flapdoodle.embed.process.runtime.Network

object MongoEmbedded {

  /*lazy val runtimeConfig = new RuntimeConfigBuilder()
    .defaults(Command.MongoD)
    .processOutput(ProcessOutput.getDefaultInstanceSilent)
    .build
*/
  lazy val mongodConfig = {
    /*val mongoPort = System.getProperty("mongoTestPort","51101").toInt
    //println("Mongo will be started at " + mongoPort)
    val mongodNetwork = new Net(mongoPort, Network.localhostIsIPv6)
    val cmdOptions = new MongoCmdOptionsBuilder().useSmallFiles(true).useNoPrealloc(true).build()
    new MongodConfigBuilder()
      .version(Version.Main.PRODUCTION)
      .net(mongodNetwork)
      .cmdOptions(cmdOptions)
      .build*/
  }

  private val counter = new AtomicInteger(0)



  @volatile var mongodExe: MongodExecutable = null
  @volatile var mongod: MongodProcess = null


  def start:Unit = synchronized {
    if(mongod == null) {
      //mongodExe = MongodStarter.getInstance(runtimeConfig).prepare(mongodConfig)
      //mongod = mongodExe.start()
    }
    counter.incrementAndGet()
  }

  def stop:Unit = synchronized {
    if(counter.decrementAndGet() == 0) {
//      println("Really stop mongo, last user exited")
      shutdownMongo()
    }
  }

  def shutdownMongo():Unit = {
    try {
      mongod.stop()
      mongod = null
    } finally {
      mongodExe.stop()
      mongodExe = null
    }
  }

  sys.addShutdownHook { () =>
    mongod.stop()
    mongodExe.stop()
  }
}