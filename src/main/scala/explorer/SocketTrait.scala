package explorer

import akka.NotUsed
import akka.http.scaladsl.model.ws._
import akka.stream.scaladsl._
import api.account.PowBlocksBroadcast
import api.account.SignUpMessagesType.SignupMessages
import block.PowBlock
import scorex.core.utils.{ActorHelper, ScorexLogging}
import io.circe.parser._
import api.circe.Codecs._
import explorer.Command.FieldRequest
import explorer.WsResponse.{InvalidCommand, Response}
import explorer.util.Codec.ToReturnPowBlock
import explorer.util.Parser

import scala.concurrent.Future

trait SocketTrait extends ActorHelper with PowBlocksBroadcast with ScorexLogging {

  def blockChainExplorer: Explorer

  private val messageFlow =
    Flow[Message]
      .collect {
        case TextMessage.Strict(t) => t
      }

  private val parsingFlow: Flow[String, WsResponse, NotUsed] = Flow[String]
    .map(decode[Command])
    .merge(producer._2)
    .mapAsync(1) {
      case pb: PowBlock =>
        log.debug(s"pb:${pb.transactions.size}")
        Future.successful(pb.toReturnPowBlock: WsResponse)
      case Left(error) =>
        log.error("Unknown message type")
        Future.successful(InvalidCommand("Unknown message type"))
      case Right(command: Command) => command match {
        case FieldRequest(request) =>

          val requestType = Parser.parseField(request)

          val response: WsResponse = if (requestType.nonEmpty) {
            log.info(s"Request from ws : ${requestType.get}")
            val exploreResult = blockChainExplorer.executeMess(requestType.get)
            log.info(s"Response for event ${exploreResult.getClass} is $exploreResult")
            exploreResult
          }
          else {
            log.error(s"Unknown request type : $request")
            Response(s"error : unknown request type -> ${request}")
          }
          Future.successful(response)
      }
    }

  import io.circe.syntax._

  private val writingFlow = Flow[WsResponse]
    .map(_.asJson.spaces2)
    .map(TextMessage(_))

  val wsFlow = messageFlow via parsingFlow via writingFlow
}
