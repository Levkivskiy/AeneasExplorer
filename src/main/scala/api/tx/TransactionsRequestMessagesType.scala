package api.tx

import api.account.SeedWithAddress

/**
  * @author luger. Created on 25.05.18.
  * @version ${VERSION}
  */
object TransactionsRequestMessagesType {
  sealed trait TxMessages

  case class GetTransactionsByAddress(address:SeedWithAddress) extends TxMessages
  case class GetCurrentTransactions() extends TxMessages
}
