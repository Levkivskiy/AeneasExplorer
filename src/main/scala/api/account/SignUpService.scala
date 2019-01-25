package api.account

import akka.NotUsed
import akka.actor.ActorRef
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.scaladsl.Flow
import akka.util.Timeout
import api.FlowError
import api.account.SignUpMessagesType.{ReturnTransactions, SignupMessages}
import api.circe.Codecs._
import block.PowBlock
import commons.SimpleBoxTransaction
import io.circe.parser.decode
import io.circe.syntax._
import scorex.core.utils.{ActorHelper, ScorexLogging}
import settings.AeneasSettings

import scala.concurrent.{Future, TimeoutException}
import scala.concurrent.duration._
/**
  * @author luger. Created on 19.03.18.
  * @version ${VERSION}
  */
trait SignUpService extends ActorHelper with PowBlocksBroadcast with ScorexLogging {
  protected def aeneasSettings:AeneasSettings
  protected def newAccActor:ActorRef
  protected def loginActor:ActorRef

  val passPhraseMixingService = new PassPhraseMixingService(aeneasSettings)

  protected [account] def flowByEventType(): Flow [Message, Message, Any] ={
    implicit val askTimeout: Timeout = Timeout(15.seconds)
    mapclientEventToAccountEvent()
      .merge(producer._2)
      .mapAsync(1){
        case pb:PowBlock =>
          log.debug(s"pb:${pb.transactions.size}")
          Future.successful(pb).map { pb =>
            TextMessage.Strict((pb.toReturnPowBlock:SignupMessages).asJson.noSpaces)
          }
        case NewAccountEvents.SentTx(tx) =>
          Future.successful(tx).map { tx =>
            TextMessage.Strict((SignUpMessagesType.ReturnTransactions(Seq(tx.json.noSpaces)):SignupMessages).asJson.noSpaces)
          }
        case x@NewAccountEvents.ErrorEvent(_) =>
          Future.successful(x).map { ev =>
            TextMessage.Strict(mapAccountEventsToMessage(ev).asJson.noSpaces)
          }
        case event@(NewAccountEvents.GetSavedSeeds() |
                    NewAccountEvents.GetSeedWithAddress(_)|
                    NewAccountEvents.Logout(_) |
                    NewAccountEvents.SendAsh(_, _, _) |
                    NewAccountEvents.SignIn (_, _)) =>
          askActor[NewAccountEvents.NewAccountEvent](loginActor, event)
            .map{x =>
              TextMessage.Strict (mapAccountEventsToMessage(x).asJson.noSpaces)
            }.recover{
            case ex:TimeoutException =>
              log.error("signup service ex:", ex)
              TextMessage.Strict ("")
          }
        case event =>
          askActor[NewAccountEvents.NewAccountEvent ](newAccActor, event)
            .map{x =>
              TextMessage.Strict ( mapAccountEventsToMessage(x).asJson.noSpaces)
            }
      }
  }

  protected [account] def mapAccountEventsToMessage (event : NewAccountEvents.NewAccountEvent):SignUpMessagesType.SignupMessages  = {
    import SignUpMessagesType._
    event match {
      case csu@NewAccountEvents.CallToSignUp(_) =>
        PassPhrase(csu.passPhrase)
      case pp@NewAccountEvents.GeneratedConfirmationPassPhrase(_) =>
        ConfirmationPassPhrase(pp.passPhrase)
      case NewAccountEvents.GeneratedSeed(Some(gs)) =>
        Seed(gs.seed)
      case NewAccountEvents.GeneratedSeed(None) =>
        ErrorResponse("data isn't correct key-pair")
      case NewAccountEvents.ReceivedPassword(seed, _) =>
        nodeViewHolder ! LoggedIn(seed.seed)
        PwdConfirmed()
      case NewAccountEvents.ErrorEvent(msg) =>
        ErrorResponse(msg)
      case NewAccountEvents.Logout(seed) =>
        nodeViewHolder ! Logout(seed)
        Logout(seed)
      case NewAccountEvents.ImportAccont(phrase) =>
        ImportAccont(phrase)
      case NewAccountEvents.SwitchToMine() =>
        nodeViewHolder ! SwitchToMine()
        SwitchToMine()//TODO send to Actor
      case NewAccountEvents.ReturnSavedSeeds(balance, height, loggedIn, seeds, lastBlocks) =>
        SavedSeeds(balance, height, loggedIn, seeds, lastBlocks.map(_.toReturnPowBlock))
      case NewAccountEvents.SignUpCancelled() =>
        CancelSignUp()
      case NewAccountEvents.ReturnSeedWithAddress(seed) =>
        ReturnSeedWithAddress(seed)
      case fail =>
        log.debug(s"failed:$fail")
        ErrorResponse("unknown event")
    }
  }

  protected [account] def mapclientEventToAccountEvent (): Flow[Message, NewAccountEvents.NewAccountEvent, NotUsed] =
    Flow[Message].collect {
      case TextMessage.Strict(t) => t
    }.map { msg =>
      import SignUpMessagesType._
      log.debug(s">>>>>>>> MSG:$msg")
      decode[SignUpMessage](msg) match {
        case Left(_) =>
          log.error("epic fail")
          NewAccountEvents.ErrorEvent("unknown message type")
        case Right(signUpMsg) => signUpMsg.msg match {
          case Signup() =>
            val phrase = passPhraseMixingService.generateRandomPassPhrase()
            NewAccountEvents.CallToSignUp(phrase)
          case CancelSignUp() =>
            NewAccountEvents.SignUpCancelled()
          case PassPhraseSaved() =>
            NewAccountEvents.SavedPassPhrase()
          case cpp@ConfirmPassPhrase(_) =>
            NewAccountEvents.ConfirmPassPhrase(cpp.passphrase)
          case slp@SetLocalPass(p1, p2) if p1 == p2 =>
            NewAccountEvents.ReceivedPassword(SeedWithAddress("", ""), slp.password)
          case SetLocalPass(_, _) =>
            NewAccountEvents
              .ErrorEvent("password equals not confirmation password")
          case login:Login =>
            NewAccountEvents.SignIn(login.seed, login.pwd)
          case SendAsh(address, _, amount, fee) =>
            NewAccountEvents.SendAsh(address, amount, fee)
          case Logout(seed) =>
            NewAccountEvents.Logout(seed)
          case ia@ImportAccont(_) =>
            NewAccountEvents.ImportAccont(ia.passPhrase)
          case GetSavedSeeds() =>
            NewAccountEvents.GetSavedSeeds()
          case GetSeedWithAddress(seed) =>
            NewAccountEvents.GetSeedWithAddress(seed)
          case _ =>
            NewAccountEvents.ErrorEvent("unknown message type")
        }
      }
    }

}
