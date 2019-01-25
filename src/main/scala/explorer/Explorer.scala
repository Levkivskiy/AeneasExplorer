package explorer

import akka.japi.Option.Some
import explorer.util.Comparator.blockEquals
import explorer.WsResponse._
import explorer.messages._
import history.AeneasHistory
import io.iohk.iodb.LSMStore
import javax.xml.ws
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.box.proposition.PublicKey25519Proposition._
import wallet.AeneasWallet

class Explorer(aeneasHistory: AeneasHistory, settings: ScorexSettings, store: LSMStore) {

  var publicWallet = AeneasWallet.readOrGenerate(aeneasHistory, settings)

  import explorer.util.Comparator._

  def executeMess(mess: RequestMessages): WsResponse =
    mess match {
      case PubKeyReq(a: PublicKey25519Proposition) => pubKeyInform(a)
      case BlockIdReq(id) => findBlock(id)
      case BlockHashReq(hash) => findBlock(hash)
      case TransactionReq(trans) => findTrans(trans)
    }

  //address balance and transaction
  def pubKeyInform(pubKey: PublicKey25519Proposition): WsResponse = {
    publicWallet.fullCheckoutInputsOutputs(Set(pubKey))
    PubKeyInfoResponse(
      publicWallet.balance().available,
      publicWallet.countMinedBlocks(pubKey),
      publicWallet.historyTransactions(pubKey).map(_.uncodeJson)
    )
  }

  import explorer.util.Codec._

  def findBlock(typeSearch: Any): WsResponse = {
    val resOpt = typeSearch match {
      case str: String => publicWallet.findBlock(_.idString equals str)
      case number: Long => {
        if (publicWallet.history.bestBlock().number < number) None
        else publicWallet.findBlock(_.number equals number)
      }
      case _ => None
    }
    if (resOpt.nonEmpty) resOpt.get.toReturnPowBlock: WsResponse
    else resOpt.getOrElse(InvalidCommand(s" Block not found : $typeSearch")).asInstanceOf[WsResponse]

  }

  def findTrans(idHash: String) = {
    val resOpt = publicWallet.findBlock(
      _.transactionPool.exists(_.idToString equals idHash))

    if (resOpt.nonEmpty) TransactionResponse(resOpt.get.transactionPool.find(_.idToString equals idHash).get.uncodeJson)
    else resOpt.getOrElse(InvalidCommand(s" Transaction not found : $idHash")).asInstanceOf[WsResponse]
  }

//  def test(pubKey: PublicKey25519Proposition) = {
//    val history = publicWallet.history
//
//    val res = history.lastBlocks(history.bestBlock().number.toInt, history.bestBlock())
//      .map(block => publicWallet.test(block, Set(pubKey))).sum
//
//    Response(res.toString)
//  }
}
