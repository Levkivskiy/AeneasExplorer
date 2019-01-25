package api.account

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import api.account.NewAccountEvents.SendAsh
import api.util.Encryption
import commons.{SimpleBoxTransaction, Value}
import history.AeneasHistory
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.LocallyGeneratedModifiersMessages.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.mainviews.NodeViewHolder
import scorex.core.mainviews.NodeViewHolder.ReceivableMessages.Subscribe
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedVault}
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.{ByteStr, ScorexLogging}
import scorex.util.encode.Base58
import scorex.crypto.hash.Sha256
import viewholder.AeneasNodeViewHolder.GetWallet
import wallet.{AeneasWallet, Balance}

import scala.concurrent.Await
import scala.util.{Failure, Success}
import scala.concurrent.duration._

/**
  * processing events from user form for Registration Flow
  *
  * @author luger. Created on 07.03.18.
  * @version ${VERSION}
  */
class LoginActor(viewHolderRef: ActorRef, aeneasHistory: AeneasHistory, settings: ScorexSettings, store: LSMStore) extends Actor with ScorexLogging {
  val logged = new AtomicBoolean(false)
  val currentSeed = new AtomicReference[ByteStr](ByteStr(Array()))
  protected var historyOpt: Option[AeneasHistory] = Some(aeneasHistory)
  protected var viewWallet: Option[AeneasWallet] = None
  implicit private val timeout: Timeout = 200 milliseconds
  private val lastBlocksCount = 20
  private var wallet: Option[AeneasWallet] = None

  override def preStart(): Unit = {
    val vhEvents = Seq(NodeViewHolder.EventType.HistoryChanged, NodeViewHolder.EventType.VaultChanged)
    viewHolderRef ! Subscribe(vhEvents)

    val future = (viewHolderRef ? GetWallet).mapTo[Option[AeneasWallet]]
    wallet = Await.result(future, timeout.duration)
  }

  def login(): Receive = {
    case NewAccountEvents.SignIn(seed, pwd) =>
      Base58.decode(seed) match {
        case Success(seedByteArray) =>
          val privateId = store.get(ByteArrayWrapper(seedByteArray))
          privateId match {
            case Some(id) =>
              Encryption.decrypt(pwd, new String(id.data, "UTF-8")) match {
                case Success(idInBase58) =>
                  log.debug(s"privateId : ${Base58.encode(Sha256(idInBase58))}")
                  if (seed == Base58.encode(Sha256(idInBase58))) {
                    val wallet = AeneasWallet.readOrGenerate(historyOpt.get, settings, ByteStr(Sha256(idInBase58)))
                    val publicKeys = wallet.publicKeys.toSeq.sortBy(_.address)
                    logged.getAndSet(true)
                    currentSeed.set(ByteStr(Sha256(idInBase58)))
                    val seedWithAddress = SeedWithAddress(seed, publicKeys.headOption.map(_.address).getOrElse(""))
                    sender() ! NewAccountEvents.ReceivedPassword(seedWithAddress, pwd)
                  } else
                    sender() ! NewAccountEvents.ErrorEvent("Account not found")
                case Failure(_) =>
                  sender() ! NewAccountEvents.ErrorEvent("Account not found")
              }
            case None =>
              sender() ! NewAccountEvents.ErrorEvent("Account not found")
          }
        case _ =>
          sender() ! NewAccountEvents.ErrorEvent("Seed is corrupted")
      }
  }

  def savedSeeds(): Receive = {
    case NewAccountEvents.GetSavedSeeds() =>
      val seeds = store.getAll().map { pair =>
        val (seed, _) = pair
        val wallet = AeneasWallet.readOrGenerate(historyOpt.get, settings, ByteStr(seed.data))
        val publicKeys = wallet.secrets.toSeq.sortBy(_.order).map(_.publicImage.address)
        SeedWithAddress(Base58.encode(seed.data), publicKeys.headOption.getOrElse(""))
      }.toList
      sender() ! NewAccountEvents.ReturnSavedSeeds(viewWallet.map(_.balance()).getOrElse(Balance(0l, 0l, 0l)), historyOpt.map(_.height).getOrElse(0), logged.get(), seeds, historyOpt.map(_.lastBlocks(lastBlocksCount, historyOpt.get.bestBlock())).getOrElse(List.empty))
  }

  def seedWithAddress(): Receive = {
    case NewAccountEvents.GetSeedWithAddress(seed) =>
      Base58.decode(seed) match {
        case Success(seedBytes) =>
          val wallet = AeneasWallet.readOrGenerate(historyOpt.get, settings, ByteStr(seedBytes))
          //log.debug(s"seedWithAddress: wallet:$wallet, publicKeys:${wallet.publicKeys}")
          val publicKeys = wallet.secrets.toSeq.sortBy(_.order).map(_.publicImage.address)
          val seedWithAddr = SeedWithAddress(seed, publicKeys.headOption.getOrElse(""))
          sender() ! NewAccountEvents.ReturnSeedWithAddress(seedWithAddr)
        case _ =>
          sender() ! NewAccountEvents.ErrorEvent("Account not found")
      }
  }

  def logout(): Receive = {
    case NewAccountEvents.Logout(seed) =>
      //TODO
      logged.getAndSet(false)
      currentSeed.getAndSet(ByteStr(Array()))
      sender() ! NewAccountEvents.Logout(seed)
  }

  def sendAsh: Receive = {
    case SendAsh(address, amount, fee) =>
      val future = (viewHolderRef ? GetWallet).mapTo[Option[AeneasWallet]]
      wallet = Await.result(future, timeout.duration)
      if (logged.get() && wallet.isDefined) {
        PublicKey25519Proposition.validPubKey(address) match {
          case Success(fundAddr) =>
            val currentWallet = wallet.get
            log.debug(s"Current balance : ${currentWallet.balance().available} GRAN.")
            if (currentWallet.balance().available <= amount + LoginActor.standartFee) {
              log.debug("Not enough funds for transaction creation!")
              sender() ! NewAccountEvents.ErrorEvent("Not enough funds for transaction creation.")
            }
            else {
              val negAmount = -amount
              val tx: SimpleBoxTransaction = new SimpleBoxTransaction(
                IndexedSeq(),
                IndexedSeq(currentWallet.publicKeys.head -> Value @@ negAmount, fundAddr -> Value @@ amount),
                IndexedSeq(),
                fee,
                System.currentTimeMillis()
              )
              viewHolderRef ! LocallyGeneratedTransaction[PublicKey25519Proposition, SimpleBoxTransaction](tx)
              log.debug(s"Sent $amount grans !")
              sender ! NewAccountEvents.SentTx(tx)
            }
          case Failure(ex) =>
            sender() ! NewAccountEvents.ErrorEvent("Account not found")
            log.error("ex:", ex)
        }
      } else
        sender() ! NewAccountEvents.ErrorEvent("Account not found")
  }

  def historyChanged: Receive = {
    case ChangedHistory(reader: AeneasHistory@unchecked) if reader.isInstanceOf[AeneasHistory] =>
      //TODO isInstanceOf & typeErasure??
      historyOpt = Some(reader)
  }

  def vaultChanged: Receive = {
    case ChangedVault(vault: AeneasWallet@unchecked) if vault.isInstanceOf[AeneasWallet] =>
      viewWallet = Option(vault)
  }

  override def receive: Receive =
    historyChanged orElse
      vaultChanged orElse
      login orElse
      savedSeeds orElse
      seedWithAddress orElse
      sendAsh orElse
      logout orElse {
      case x =>
        log.error(s"Unknown event type $x")
        sender() ! NewAccountEvents.ErrorEvent("Unknown event type")
    }
}

object LoginActor {
  val standartFee = 100000L
}
