package network

import akka.actor.ActorRef
import history.sync.AeneasSynchronizer.RequestPeerManager
import scorex.core.network.{NetworkController, UPnP}
import scorex.core.network.message.MessageHandler
import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkTimeProvider

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 21.03.18.
  */
class AeneasNetwork (settings: NetworkSettings,
                     messageHandler: MessageHandler,
                     upnp: UPnP,
                     peerManagerRef: ActorRef,
                     timeProvider: NetworkTimeProvider,
                    ) extends NetworkController(settings, messageHandler, upnp, peerManagerRef, timeProvider) {

   // галимый костыль, переделать к чертям. Вместе со Скорексом.
   def requestPeerManager : Receive = {
      case RequestPeerManager =>
         log.debug(s"AeneasNetwork : Peer was requested.")
         sender() ! peerManagerRef
   }

   override def receive: Receive = {
      requestPeerManager orElse
      super.receive
   }
}

