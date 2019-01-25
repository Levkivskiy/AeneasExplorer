package api.account

import akka.actor.ActorRef
import block.{AeneasBlock, PowBlock}
import commons.SimpleBoxTransaction
import wallet.Balance

/**
  * @author luger. Created on 07.03.18.
  * @version ${VERSION}
  */
object NewAccountEvents{
  sealed trait NewAccountEvent
  case class ReceivedPassword(seed:SeedWithAddress, pwd: String) extends NewAccountEvent
  case class CallToSignUp (passPhrase:List[String]) extends NewAccountEvent
  case class SignUpCancelled () extends NewAccountEvent
  case class BackupPassPhrase() extends NewAccountEvent
  case class GeneratedConfirmationPassPhrase(passPhrase: List[String]) extends NewAccountEvent
  case class GeneratedSeed(userKey: Option[UserKey]) extends NewAccountEvent
  case class ConfirmationOutActorRef(ref:ActorRef) extends NewAccountEvent
  case class SeedOutActorRef(ref:ActorRef) extends NewAccountEvent
  case class ConfirmPassPhrase(passPhrase: List[String]) extends NewAccountEvent
  case class SavedPassPhrase() extends NewAccountEvent
  case class ErrorEvent (msg:String) extends NewAccountEvent
  case class ImportAccont(passPhrase: List[String]) extends NewAccountEvent
  case class SwitchToMine() extends NewAccountEvent
  case class GetSeedWithAddress (publicSeed:String) extends NewAccountEvent
  case class ReturnSeedWithAddress (seed:SeedWithAddress) extends NewAccountEvent
  case class SignIn (publicSeed:String, pwd:String) extends NewAccountEvent
  case class GetSavedSeeds () extends NewAccountEvent
  case class SendAsh (address:String, amount:Long, fee:Long) extends NewAccountEvent
  case class SentTx (tx:SimpleBoxTransaction) extends NewAccountEvent
  case class ReturnSavedSeeds (balance:Balance, height:Long, loggedIn:Boolean, seeds:List[SeedWithAddress], lastBlocks: Seq[PowBlock]) extends NewAccountEvent
  case class Logout (publicSeed:String) extends NewAccountEvent
}

case class SeedWithAddress(seed:String, address:String)