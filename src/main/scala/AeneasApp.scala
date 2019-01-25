import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import network.AeneasNetwork
import network.messagespec._
import scorex.core.api.http.{ApiRoute, CompositeHttpService}
import scorex.core.mainviews.{NodeViewHolder, PersistentNodeViewModifier}
import scorex.core.network.{NetworkController, UPnP}
import scorex.core.network.message._
import scorex.core.network.peer.PeerManager
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * File was copied from scorex.core.app.Application.scala and extended.
  * Created on 20.03.18.
  * Custom ApplicationSpec for Aeneas blockchain.
  */
trait AeneasApp extends ScorexLogging {
   type P <: Proposition
   type TX <: Transaction[P]
   type PMOD <: PersistentNodeViewModifier
   type NVHT <: NodeViewHolder[P, TX, PMOD]

   val ApplicationNameLimit = 50

   // settings
   implicit val settings: ScorexSettings

   // api
   val apiRoutes: Seq[ApiRoute]

   protected implicit lazy val actorSystem = ActorSystem(settings.network.agentName)

   val nodeViewHolderRef: ActorRef
   val nodeViewSynchronizer: ActorRef
   val localInterface: ActorRef

   // network initialization.
   val timeProvider = new NetworkTimeProvider(settings.ntp)
   val peerManagerRef: ActorRef = actorSystem.actorOf(Props(new PeerManager(settings, timeProvider)))

   val nProps = Props(new AeneasNetwork(settings.network, messagesHandler, upnp, peerManagerRef, timeProvider))
   val networkControllerRef: ActorRef = actorSystem.actorOf(nProps, "networkController")
   log.debug(s"Network controller was started : ${networkControllerRef.toString}")


   // p2p
   lazy val upnp = new UPnP(settings.network)

   protected val additionalMessageSpecs: Seq[MessageSpec[_]]
   val swaggerConfig = ""


   private lazy val basicSpecs = {
      val invSpec = new InvSpec(settings.network.maxInvObjects)
      val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
      Seq(
         GetPeersSpec,
         PeersSpec,
         invSpec,
         requestModifierSpec,
         ModifiersSpec
      )
   }

   protected lazy val downloadSpecs = {
      val chainSpec = new FullBlockChainRequestSpec
      val blockMessageSpec = new PoWBlockMessageSpec
      val batchMessageSpec = new PoWBlocksMessageSpec
      val endDownloadSpec = new EndDownloadSpec
      val invSpec = new DownloadInvSpec(settings.network.maxInvObjects)
      Seq (chainSpec, blockMessageSpec, batchMessageSpec, endDownloadSpec, invSpec)
   }

   protected lazy val consensusSpec = {
      val cbBlockSpec = new ConsensusBestBlockSpec()
      val cbNotFoundBlockSpec = new ConsensusBestBlockNotFoundSpec ()
      Seq (cbBlockSpec, cbNotFoundBlockSpec)
   }

   lazy val combinedRoute = CompositeHttpService(actorSystem, apiRoutes, settings.restApi, swaggerConfig).compositeRoute
   lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs ++ downloadSpecs ++ consensusSpec)
  
   def run(): Unit = {
      require(settings.network.agentName.length <= ApplicationNameLimit)

      log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
      log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
      log.debug(s"RPC is allowed at ${settings.restApi.bindAddress.toString}")

      implicit val materializer = ActorMaterializer()
      Http().bindAndHandle(combinedRoute, settings.restApi.bindAddress.getAddress.getHostAddress, settings.restApi.bindAddress.getPort)

      //on unexpected shutdown
      Runtime.getRuntime.addShutdownHook(new Thread() {
         override def run() {
            log.error("Unexpected shutdown")
            synchronized {
               log.info("Stopping network services")
               if (settings.network.upnpEnabled) upnp.deletePort(settings.network.bindAddress.getPort)
                  networkControllerRef ! NetworkController.ReceivableMessages.ShutdownNetwork

               log.info("Stopping actors (incl. block generator)")
               actorSystem.terminate().onComplete { _ =>

                  log.info("Exiting from the app...")
                  System.exit(0)
               }
            }
         }
      })
   }
}