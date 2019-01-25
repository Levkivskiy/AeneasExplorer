package api.tx

import api.account.SeedWithAddress
import commons.SimpleBoxTransaction

/**
  * @author luger. Created on 25.05.18.
  * @version ${VERSION}
  */
object TransactionsResponseMessagesType {
  sealed trait TxResponseMessages

  case class ReturnTransactionsByAddress(txs:List[SimpleBoxTransaction]) extends TxResponseMessages
  case class GetCurrentTransactions() extends TxResponseMessages
}
