package api.account

import java.time.{LocalDateTime, ZoneOffset}

import akka.actor.Actor
import api.util.Encryption
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.utils.ScorexLogging
import scorex.util.encode.Base58
import scorex.crypto.hash.Sha256

import scala.collection.mutable
import scala.util.{Random, Success}

/**
  * processing events from user form for Registration Flow
  * @author luger. Created on 07.03.18.
  * @version ${VERSION}
  */
class NewAccActor(store:LSMStore) extends Actor with ScorexLogging{
  private var currentPassPhrase = List.empty[String]
  private var shufflePassPhrase = List.empty[String]
  private var signUpStarted = false
  private var passPhraseSavedByUser = false
  private val userKeySet = mutable.HashSet[UserKey]()


  private def signup:Receive = {
    case NewAccountEvents.CallToSignUp(passPhrase) =>
      log.debug("Call To Sign Up")
      if (!passPhraseSavedByUser) {
        signUpStarted = true
        currentPassPhrase = passPhrase
        sender() ! NewAccountEvents.CallToSignUp(passPhrase)
      }else sender() ! NewAccountEvents.ErrorEvent("already initialized signing up")
  }

  private def signupCancellation:Receive = {
    case NewAccountEvents.SignUpCancelled() =>
      log.debug("Sign Up Cancelled by user ")
      signUpStarted = false
      passPhraseSavedByUser = false
      currentPassPhrase = List.empty
      shufflePassPhrase = List.empty
      sender() ! NewAccountEvents.SignUpCancelled()
  }

  private def savedPassPhrase:Receive = {
    case NewAccountEvents.SavedPassPhrase() =>
      log.debug(s"Pass Phrase Saved By User;$signUpStarted")
      if (signUpStarted) {
        passPhraseSavedByUser = true
        shufflePassPhrase = Random.shuffle(currentPassPhrase)
        log.debug(s"shufflePassPhrase: $shufflePassPhrase")
        sender() ! NewAccountEvents.GeneratedConfirmationPassPhrase(shufflePassPhrase)
      }else sender() ! NewAccountEvents.ErrorEvent("")
  }

  private def confirmPassPhrase:Receive = {
    case NewAccountEvents.ConfirmPassPhrase(confirmPhraseSeq) =>
      log.debug(s"Pass Phrase Confirmed by User : $confirmPhraseSeq, ${currentPassPhrase == confirmPhraseSeq}")
      if (currentPassPhrase == confirmPhraseSeq){
        val privateId = Base58.encode(Sha256(currentPassPhrase.mkString(",").getBytes("UTF-8")))
        val publicSeed = Base58.encode(Sha256(privateId))
        log.debug(s"privateId:$privateId, seed:$publicSeed")
        userKeySet += UserKey (publicSeed, privateId)
        log.debug(s"seed set:$userKeySet")
        signUpStarted = false
        passPhraseSavedByUser = false
        sender() ! NewAccountEvents.GeneratedSeed(userKeySet.headOption)
      }else sender() ! NewAccountEvents.ErrorEvent("password equals not confirmation password")
  }

  /**
    *
    * @return
    */
  private def receivedPassword:Receive = {
    case NewAccountEvents.ReceivedPassword(_, pwd) =>
      log.debug(s"password : $pwd")
      currentPassPhrase = List.empty
      shufflePassPhrase = List.empty
      userKeySet.headOption.map{x =>
        val UserKey (publicSeed, privateId) = x
        (Base58.decode(publicSeed), privateId)
      } match {
        case Some ((Success(publicSeed), privateId)) =>
          log.debug(s"seed:$publicSeed, privateId:$privateId, ${Encryption.encrypt(pwd, privateId)}")
          store.update(
            ByteArrayWrapper(publicSeed ++ String.valueOf(LocalDateTime.now().toEpochSecond(ZoneOffset.UTC)).getBytes()), Seq(),
            Seq (ByteArrayWrapper(publicSeed) -> ByteArrayWrapper(Encryption.encrypt(pwd, privateId).getBytes("UTF-8"))))
          sender() ! NewAccountEvents.ReceivedPassword(SeedWithAddress (Base58.encode(publicSeed), ""), pwd)
          userKeySet.clear()
        case _ =>
          sender() ! NewAccountEvents.ErrorEvent("Seed or PrivateId is corrupted")
      }
  }

  def importAccount ():Receive = {
    case NewAccountEvents.ImportAccont(passPhrase) =>
      val privateId = Base58.encode(Sha256(passPhrase.mkString(",")))
      val publicSeed = Base58.encode(Sha256(privateId))
      userKeySet += UserKey (publicSeed, privateId)
      signUpStarted = false
      passPhraseSavedByUser = false
      sender() ! NewAccountEvents.GeneratedSeed(userKeySet.headOption)
  }


  override def receive: Receive =
    signup orElse
      signupCancellation orElse
      savedPassPhrase orElse
      confirmPassPhrase orElse
      importAccount orElse
      receivedPassword orElse {
      case x =>
        log.error(s"Unknown event type $x")
        sender() ! NewAccountEvents.ErrorEvent("Unknown event type")
    }
}


case class UserKey (seed:String, key:String)