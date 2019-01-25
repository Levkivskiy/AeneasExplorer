package explorer

import commons.SimpleBoxTransaction
import explorer.Command.FieldRequest
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.JsonCodec
import io.circe.syntax._
import io.circe.generic.auto._

sealed trait Command

object Command {

  case class FieldRequest(request: String) extends Command

  implicit val decodeCommand: Decoder[Command] =
    Decoder[FieldRequest].map[Command](identity)
  //.or(Decoder[C].map[A](identity))
  implicit val encodeCommand: Encoder[Command] = Encoder.instance {
    case fieldRequest@FieldRequest(_) => fieldRequest.asJson
  }

}

sealed trait WsResponse

object WsResponse {

  case class InvalidCommand(error: String) extends WsResponse

  // only for test
  case class Response(msg: String) extends WsResponse

  //json with space2
  case class PubKeyInfoResponse(balance: Long, mined: Int, transactions: Seq[Json])
    extends WsResponse

  case class TransactionResponse(transaction: Json) extends WsResponse

  case class ReturnPowBlock(
                             blockHeight: Long,
                             id: String,
                             parentId: String,
                             address: String,
                             timestamp: Long,
                             nonce: Long,
                             root: String,
                             transactions: Seq[String]
                           ) extends WsResponse


  implicit val decodeWsResponse: Decoder[WsResponse] =
    Decoder[Response].map[WsResponse](identity)
      .or(Decoder[InvalidCommand].map[WsResponse](identity))
      .or(Decoder[ReturnPowBlock].map[WsResponse](identity))
      .or(Decoder[PubKeyInfoResponse].map[WsResponse](identity))
      .or(Decoder[TransactionResponse].map[WsResponse](identity))

  implicit val encodeWsResponse: Encoder[WsResponse] = Encoder.instance {
    case response@Response(_) => response.asJson
    case invalidCommand@InvalidCommand(_) => invalidCommand.asJson
    case returnPowBlock@ReturnPowBlock(_, _, _, _, _, _, _, _) => returnPowBlock.asJson
    case pubKeyInfoResponse@PubKeyInfoResponse(_, _, _) => pubKeyInfoResponse.asJson
    case transactionResponse@TransactionResponse(_) => transactionResponse.asJson
  }
}