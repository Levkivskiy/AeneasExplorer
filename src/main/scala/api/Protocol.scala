package api

/**
  * @author luger. Created on 06.03.18.
  * @version ${VERSION}
  */
object Protocol {
  sealed trait Message
  case class CreateAccount(password:String) extends Message
  case class ImportAccount(publicId:String) extends Message
  case class ViewTransactions() extends Message
  case class GetBlockhainInfo() extends Message
  case class GetWalletInfo() extends Message
}
