package api

import java.io.{File, FileInputStream}
import java.security.{KeyStore, SecureRandom}
import java.util.concurrent.Executors

import javax.net.ssl.{KeyManagerFactory, SSLContext}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.{ConnectionContext, Http}
import akka.stream.ActorMaterializer
import api.account.PowBlocksBroadcast
import io.iohk.iodb.LSMStore
import settings.AeneasSettings

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.io.Source
import scala.util.{Failure, Success}
import akka.http.scaladsl.server.Directives._
import scorex.core.utils.ScorexLogging

/**
  * @author luger. Created on 09.03.18.
  * @version ${VERSION}
  */
class WsServerRunner(nodeViewHolderRef:ActorRef, aeneasSettings: AeneasSettings)(implicit system:ActorSystem) extends ScorexLogging{
  private implicit val materializer: ActorMaterializer = ActorMaterializer()


  private val storage: LSMStore = {
    val wFile = new File (aeneasSettings.scorexSettings.dataDir.getAbsolutePath + File.separator + "account")
    if (!wFile.exists) wFile.mkdirs()
    new LSMStore(wFile, maxJournalEntryCount = 10000)
  }

  private implicit val exectutionContext: ExecutionContextExecutorService = {
    val pool = Executors.newCachedThreadPool()
    ExecutionContext.fromExecutorService(pool)
  }


  def run = {
    val wsApi = new AeneasApi(nodeViewHolderRef, aeneasSettings, storage)
    val static = new StaticRoutes(aeneasSettings)
    val addr = aeneasSettings.wsApiSettings.bindAddress
    val bind = Http().bindAndHandle(wsApi.route ~ static.generate, addr.getHostName, addr.getPort)//, connectionContext = ServerContext(aeneasSettings).context) TODO add correct wss support
    bind.onComplete {
      case Success(binding) =>
        val localAddress = binding.localAddress
        log.info(s"WebSocket Server is listening on ${localAddress.getHostName}:${localAddress.getPort}")

        val htmlFile = new File("index.html")
        import java.awt.Desktop
        Desktop.getDesktop.browse(htmlFile.toURI)

      case Failure(e) =>
        log.debug(s"Binding failed with ${e.getMessage}")
        sys.exit(-1)
    }
  }

}

class ServerContext(val aeneasSettings:AeneasSettings){

  val context : ConnectionContext = {
    val password = aeneasSettings.wsApiSettings.keyPwd.toCharArray
    val context = SSLContext.getInstance("TLS")
    val ks = KeyStore.getInstance("PKCS12")
    val keyFileStream = new FileInputStream(aeneasSettings.wsApiSettings.keyPath)

    ks.load(keyFileStream, password)
    val keyManagerFactory = KeyManagerFactory.getInstance("SunX509")
    keyManagerFactory.init(ks, password)
    context.init(keyManagerFactory.getKeyManagers, null, new SecureRandom)
    // start up the web server
    ConnectionContext.https(context)
  }
}

object ServerContext {
  def apply(aeneasSettings: AeneasSettings): ServerContext = new ServerContext(aeneasSettings)
}
