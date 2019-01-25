package explorer.util

import explorer.messages._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition.validPubKey
import scorex.core.utils.ScorexLogging

import scala.util.Try

object Parser extends ScorexLogging{

  val blockHashLength = 43
  val transIdLength = 44

  def parseField(field: String): Option[RequestMessages] = field match {
    case pubKey: String if validPubKey(pubKey).isSuccess =>
      Some(PubKeyReq(validPubKey(pubKey).get))
    case idBlock: String if Try(idBlock.toLong).isSuccess =>
      Some(BlockIdReq(idBlock.toLong))
    case hashBlock: String if hashBlock.length == blockHashLength =>
      Some(BlockHashReq(hashBlock))
    case transId: String if transId.length == transIdLength =>
      Some(TransactionReq(transId))
    case err => {
      log.info(s"None parsing ${err}")
      None
    }


  }
}
