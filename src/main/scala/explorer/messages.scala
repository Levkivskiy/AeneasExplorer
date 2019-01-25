package explorer

import scorex.core.transaction.box.proposition.PublicKey25519Proposition

object messages {

  trait RequestMessages

  case class PubKeyReq(pubKey: PublicKey25519Proposition) extends RequestMessages

  case class BlockIdReq(long: Long) extends RequestMessages

  case class BlockHashReq(hash: String) extends RequestMessages

  case class TransactionReq(id: String) extends RequestMessages

}

object WalletMesseges {

  case class FindBlockId(id: Int)

  case class FindBlockHash(hash: String)

  case class CountBalance(pubKeySet: PublicKey25519Proposition)

  case class CountMined(pubKeySet: PublicKey25519Proposition)

  case class HistoryTransaction(pubKeySet: PublicKey25519Proposition)

  case class FindTransaction(idHash: String)
}
