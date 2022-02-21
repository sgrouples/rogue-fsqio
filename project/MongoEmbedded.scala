import java.util.concurrent.atomic.AtomicInteger
import de.flapdoodle.embed.mongo._
import de.flapdoodle.embed.mongo.MongodStarter
import de.flapdoodle.embed.mongo.config._
import de.flapdoodle.embed.mongo.distribution.Version
import de.flapdoodle.embed.mongo.packageresolver.Command
import de.flapdoodle.embed.process.config.process.ProcessOutput
import de.flapdoodle.embed.process.runtime.Network
import org.slf4j.LoggerFactory


object MongoEmbedded {
  val logger = LoggerFactory.getLogger(getClass().getName())
  val runtimeConfig = Defaults.runtimeConfigFor(Command.MongoD, logger)
    .processOutput(ProcessOutput.silent())
    .build();

  val starter = MongodStarter.getInstance(runtimeConfig)

  lazy val mongodConfig = {
    val mongoPort = System.getProperty("mongoTestPort","51101").toInt
    //println("Mongo will be started at " + mongoPort)
    val mongodNetwork = new Net(mongoPort, Network.localhostIsIPv6)
    val cmdOptions = MongoCmdOptions.builder
      .useSmallFiles(true)
      .useNoPrealloc(true)
      .isVerbose(false)
      .build()
    MongodConfig.builder
      .version(Version.Main.PRODUCTION)
      .net(mongodNetwork)
      .cmdOptions(cmdOptions)
      .build
  }

  private val counter = new AtomicInteger(0)



  @volatile var mongodExe: MongodExecutable = null
  @volatile var mongod: MongodProcess = null


  def start:Unit = synchronized {
    if(mongod == null) {
      mongodExe = starter.prepare(mongodConfig);
      mongod = mongodExe.start();
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
      if(mongod != null) {
        mongod.stop()
        mongod = null
      }
    } finally {
      if(mongod != null) {
        mongodExe.stop()
      }
      mongodExe = null
    }
  }

  sys.addShutdownHook { () =>
    mongod.stop()
    mongodExe.stop()
  }
}