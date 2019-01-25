package scorex.core.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface, URI}

import akka.actor._
import akka.io.Tcp.SO.KeepAlive
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import scorex.core.network.NetworkController.ReceivableMessages._
import scorex.core.network.PeerConnectionHandler.ReceivableMessages.CloseConnection
import scorex.core.network.message.{Message, MessageHandler, MessageSpec}
import scorex.core.network.peer.PeerManager.EventType
import scorex.core.network.peer.PeerManager.ReceivableMessages.{CheckPeers, Disconnected, FilterPeers, Subscribe}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

/**
  * Control all network interaction
  * must be singleton
  */
class NetworkController(settings: NetworkSettings,
                        messageHandler: MessageHandler,
                        upnp: UPnP,
                        peerManagerRef: ActorRef,
                        timeProvider: NetworkTimeProvider
                       ) extends Actor with ScorexLogging {

  import NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer

  private implicit val system: ActorSystem = context.system

  private val peerSynchronizer: ActorRef = PeerSynchronizerRef("PeerSynchronizer", self, peerManagerRef, settings)

  private implicit val timeout: Timeout = Timeout(settings.controllerTimeout.getOrElse(5 seconds))

  private val messageHandlers = mutable.Map[Seq[Message.MessageCode], ActorRef]()

  // map socket address for each current connection to ConnectionHandlerRef
  private val peerConnectionHandlers = mutable.Map[InetSocketAddress, ActorRef]()

  private val tcpManager = IO(Tcp)

  //check own declared address for validity
  if (!settings.localOnly) {
    settings.declaredAddress.foreach { myAddress =>
      Try {
        val uri = new URI("http://" + myAddress)
        val myHost = uri.getHost
        val myAddrs = InetAddress.getAllByName(myHost)

        NetworkInterface.getNetworkInterfaces.asScala.exists { intf =>
          intf.getInterfaceAddresses.asScala.exists { intfAddr =>
            val extAddr = intfAddr.getAddress
            myAddrs.contains(extAddr)
          }
        } match {
          case true => true
          case false =>
            if (settings.upnpEnabled) {
              val extAddr = upnp.externalAddress
              myAddrs.contains(extAddr)
            } else false
        }
      }.recover { case t: Throwable =>
        log.error("Declared address validation failed: ", t)
      }
    }
  }

  lazy val localAddress = settings.bindAddress

  //an address to send to peers
  lazy val externalSocketAddress: Option[InetSocketAddress] = {
    settings.declaredAddress orElse {
      if (settings.upnpEnabled) {
        upnp.externalAddress.map(a => new InetSocketAddress(a, settings.bindAddress.getPort))
      } else None
    }
  }

  log.info(s"Declared address: $externalSocketAddress")


  lazy val connTimeout = Some(settings.connectionTimeout)

  //bind to listen incoming connections
  tcpManager ! Bind(self, localAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  private def bindingLogic: Receive = {
    case Bound(_) =>
      log.info("Successfully bound to the port " + settings.bindAddress.getPort)
      context.system.scheduler.schedule(600.millis, 5.seconds)(peerManagerRef ! CheckPeers)

    case CommandFailed(_: Bind) =>
      log.error("Network port " + settings.bindAddress.getPort + " already in use!")
      context stop self
    //TODO catch?
  }

  def businessLogic: Receive = {
    //a message coming in from another peer
    case Message(spec, Left(msgBytes), Some(remote)) =>
      val msgId = spec.messageCode

      spec.parseBytes(msgBytes) match {
        case Success(content) =>
          messageHandlers.find(_._1.contains(msgId)).map(_._2) match {
            case Some(handler) =>
              handler ! DataFromPeer(spec, content, remote)

            case None =>
              log.error("No handlers found for message: " + msgId)
            //todo: ban a peer
          }
        case Failure(e) =>
          log.error("Failed to deserialize data: ", e)
        //todo: ban peer
      }

    case SendToNetwork(message, sendingStrategy) =>
      log.debug(s"sendToNetwork:$message")
      (peerManagerRef ? FilterPeers(sendingStrategy))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach{x =>
          log.debug(s"handlerRef: ${x.handlerRef.path}, message: ${message}")
          x.handlerRef ! message})
  }

  val outgoing = mutable.Set[InetSocketAddress]()

  def peerLogic: Receive = {
    case ConnectTo(remote) =>
      log.info(s"Connecting to: $remote")
      outgoing += remote
      tcpManager ! Connect(remote,
                          localAddress = externalSocketAddress,
                          options = KeepAlive(true) :: Nil,
                          timeout = connTimeout,
                          pullMode = true) //todo: check pullMode flag

    case DisconnectFrom(peer) =>
      log.info(s"Disconnected from ${peer.socketAddress}")
      peer.handlerRef ! CloseConnection
      peerManagerRef ! Disconnected(peer.socketAddress)
      peerConnectionHandlers.get(peer.socketAddress) match {
         case Some(address) => context.stop(address)
         case _ =>
      }
      // delete socket address with all mappings from peerConnectionHandlers
      peerConnectionHandlers - peer.socketAddress

    case Blacklist(peer) =>
      peer.handlerRef ! PeerConnectionHandler.ReceivableMessages.Blacklist
      // todo: the following message might become unnecessary if we refactor PeerManager to automatically
      // todo: remove peer from `connectedPeers` on receiving `AddToBlackList` message.
      peerManagerRef ! Disconnected(peer.socketAddress)

    case Connected(remote, local) =>
      val direction = if(outgoing.contains(remote)) Outgoing else Incoming
      val logMsg = direction match {
        case Incoming => s"New incoming connection from $remote established (bound to local $local)"
        case Outgoing => s"New outgoing connection to $remote established (bound to local $local)"
      }
      log.info(logMsg)
      val connection = sender()
      val handlerProps: Props = PeerConnectionHandlerRef.props(settings, self, peerManagerRef,
        messageHandler, connection, direction, externalSocketAddress, remote, timeProvider)
      peerConnectionHandlers.put(remote, context.actorOf(handlerProps)) // launch connection handler
      outgoing -= remote

    case CommandFailed(c: Connect) =>
      outgoing -= c.remoteAddress
      log.debug("Failed to connect to : " + c.remoteAddress)
      peerManagerRef ! Disconnected(c.remoteAddress)
  }

  //calls from API / application
  def interfaceCalls: Receive = {
    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      (peerManagerRef ? FilterPeers(Broadcast))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! CloseConnection))
      self ! Unbind
      context stop self

    case SubscribePeerManagerEvent(events) =>
      peerManagerRef ! Subscribe(sender(), events)

    case AcquirePeerHandler(address)=>
     peerConnectionHandlers.get(address) match {
       case Some(actorRef) =>
         sender() ! actorRef

       case None =>
     }
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse interfaceCalls orElse {
    case RegisterMessagesHandler(specs, handler) =>
      log.info(s"Registering handlers for ${specs.map(s => s.messageCode -> s.messageName)}")
      messageHandlers += specs.map(_.messageCode) -> handler

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController: got something strange $nonsense")
  }

  override def postStop(): Unit = {
    log.debug("I'm died! :(")
    super.postStop()
  }
}

object NetworkController {
   object ReceivableMessages {
      case class AcquirePeerHandler(address: InetSocketAddress)

      case class RegisterMessagesHandler(specs: Seq[MessageSpec[_]], handler: ActorRef)
      case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)
      case object ShutdownNetwork
      case class ConnectTo(address: InetSocketAddress)
      case class DisconnectFrom(peer: ConnectedPeer)
      case class Blacklist(peer: ConnectedPeer)
      case class SubscribePeerManagerEvent(events: Seq[EventType])
  }
}

object NetworkControllerRef {
  def props(settings: NetworkSettings,
            messageHandler: MessageHandler,
            upnp: UPnP,
            peerManagerRef: ActorRef,
            timeProvider: NetworkTimeProvider): Props =
    Props(new NetworkController(settings, messageHandler, upnp, peerManagerRef, timeProvider))

  def apply(settings: NetworkSettings,
            messageHandler: MessageHandler,
            upnp: UPnP,
            peerManagerRef: ActorRef,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(settings, messageHandler, upnp, peerManagerRef, timeProvider))

  def apply(name: String,
            settings: NetworkSettings,
            messageHandler: MessageHandler,
            upnp: UPnP,
            peerManagerRef: ActorRef,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(settings, messageHandler, upnp, peerManagerRef, timeProvider), name)
}
