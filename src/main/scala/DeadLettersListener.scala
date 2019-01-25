import akka.actor.{Actor, ActorRef, DeadLetter}
import akka.event.Logging.Info
import network.BlockchainDownloader.FailedHistory
import network.messagespec.DownloadInvSpec
import scorex.core.ModifierId
import scorex.core.network.message.Message
import scorex.core.settings.NetworkSettings
import scorex.util.ScorexLogging

/**
  * @author luger. Created on 07.12.18.
  * @version ${VERSION}
  */
class DeadLettersListener(viewHolder:ActorRef, networkSettings: NetworkSettings) extends Actor with ScorexLogging{
  val invSpec = new DownloadInvSpec(networkSettings.maxInvObjects)
  private val eventStream = context.system.eventStream
  private val maxCount = context.system.settings.LogDeadLetters

  override def preStart(): Unit =
    eventStream.subscribe(self, classOf[DeadLetter])

  // don't re-subscribe, skip call to preStart
  override def postRestart(reason: Throwable): Unit = ()

  // don't remove subscription, skip call to postStop, no children to stop
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = ()

  override def postStop(): Unit =
    eventStream.unsubscribe(self)

  def receive = {
    case DeadLetter(message, snd, rcp) =>
      log.error(s"Dead letter : sender : $snd, message : $message")
      message match {
        case Message(spec, Right((_, a:Seq[ModifierId])), _) =>
          if (invSpec.messageCode == spec.messageCode){
            log.debug("InvSpec dead letter")
            a.headOption.foreach(viewHolder ! FailedHistory(_))
          }
        case _ =>
      }
  }

}
