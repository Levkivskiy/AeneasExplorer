/*
 * Copyright 2018, Aeneas Platform.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package viewholder

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import akka.actor.ActorRef
import api.account.SignUpMessagesType.{LoggedIn, Logout, SwitchToMine}
import block.{AeneasBlock, PowBlock, PowBlockCompanion}
import commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool, SimpleBoxTransactionSerializer, Value}
import history.AeneasHistory
import history.sync.AeneasSynchronizer.{PreStartDownloadRequest, PreStartDownloadRequestFull, SynchronizerAlive}
import history.sync.VerySimpleSyncInfo
import mining.Miner.{MinerAlive, StartMining, StopMining, UiInformatorSubscribe}
import network.BlockchainDownloader.{DownloadEnded, FailedHistory}
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.mainviews.NodeViewHolder.{CurrentView, EventType}
import scorex.core.mainviews.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, ModifiersFromRemote, RemoteBestBlockModifier}
import scorex.core.mainviews.{NodeViewHolder, NodeViewModifier}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, FailedTransaction, NodeViewHolderEvent, SuccessfulTransaction}
import scorex.core.serialization.Serializer
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.{ByteStr, ScorexLogging}
import scorex.util.encode.Base58
import settings.SimpleMiningSettings
import state.SimpleMininalState
import viewholder.AeneasNodeViewHolder.{AeneasSubscribe, GetWallet, NodeViewEvent, NotifySubscribersOnRestore}
import wallet.AeneasWallet

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import AeneasNodeViewHolder.GetWallet
import explorer.WalletMesseges._
import explorer.messages._
import scorex.core.LocallyGeneratedModifiersMessages.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.consensus.HistoryReader

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 19.01.18.
  */
//noinspection ScalaStyle
class AeneasNodeViewHolder(settings: ScorexSettings, minerSettings: SimpleMiningSettings)
  extends NodeViewHolder[PublicKey25519Proposition, SimpleBoxTransaction, AeneasBlock]
    with ScorexLogging {
  type P = PublicKey25519Proposition
  type TX = SimpleBoxTransaction
  override type SI = VerySimpleSyncInfo
  override type HIS = AeneasHistory
  override type MS = SimpleMininalState
  override type VL = AeneasWallet
  override type MP = SimpleBoxTransactionMemPool
  override type NodeView = (HIS, MS, Option[VL], MP)

  private lazy val synchronizerStatus: AtomicBoolean = new AtomicBoolean(false)
  private lazy val minerStatus: AtomicBoolean = new AtomicBoolean(false)
  private lazy val minerActivation: AtomicBoolean = new AtomicBoolean(false)
  private lazy val prestartDownloadEnded: AtomicBoolean = new AtomicBoolean(false)
  private lazy val logged: AtomicBoolean = new AtomicBoolean(false)
  private var clientInformatorRef: Option[ActorRef] = None

  private def checkShouldUpdate(): Boolean =
    AeneasHistory.readOrGenerate(settings, minerSettings).height <= 0

  /**
    * Restore a local view during a node startup.
    * If no any stored view or other peers in network found, None is to be returned.
    *
    * If it is the first launch, history should be empty and
    * specific signal will be send to synchronizer.
    */
  override def restoreState(): Option[NodeView] = {
    minerActivation.set(false)
    if (checkShouldUpdate()) None
    else {
      prestartDownloadEnded.getAndSet(true)
      AeneasWallet.walletFile(settings)
      log.debug(s"AeneasWallet.exists : ${AeneasWallet.exists(settings)}")
      val history = AeneasHistory.readOrGenerate(settings, minerSettings)
      val minState = SimpleMininalState.readOrGenerate(settings)
      val wallet = Some(AeneasWallet.readOrGenerate(history, settings, 1))
      val memPool = SimpleBoxTransactionMemPool.emptyPool

      log.debug(s"AeneasViewHolder.restoreState : history length is ${history.height}, bestBlock:${if (!history.isEmpty) history.bestBlock().copy(transactionPool = Seq.empty) else ""}")

      def printHistory(id: ModifierId): Unit = {
        history.storage.modifierById(id) match {
          case Some(block: PowBlock) =>
            log.debug(s"block:${block.copy(transactionPool = Seq.empty).json}, txs size:${block.transactions.size}")
            printHistory(block.parentId)
          case _ =>
        }
      }

      printHistory(history.bestBlock().id)
      log.debug(s"best block:${history.bestBlock().json}")
      Some(history, minState, wallet, memPool)
    }
  }

  /**
    * Restore a local view during a node startup.
    * If no any stored view or other peers in network found, None is to be returned.
    *
    * If it is the first launch, history should be empty and
    * specific signal will be send to synchronizer.
    */

  def updateChainState(): Option[NodeView] = {
    self ! NotifySubscribersOnRestore

    // should be empty
    val history = AeneasHistory.readOrGenerate(settings, minerSettings)
    val minState = SimpleMininalState.readOrGenerate(settings)
    val wallet = Some(AeneasWallet.readOrGenerate(history, settings, 1))
    val memPool = SimpleBoxTransactionMemPool.emptyPool
    log.debug(s" height:${history.height}, ${if (history.isEmpty) "" else history.bestBlock().json.toString()}")
    notifyAeneasSubscribers(NodeViewEvent.UpdateHistory, ChangedHistory(history))
    Some(history, minState, wallet, memPool)
  }

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    * It doesn't care for user-nodes.
    */
  override protected def genesisState: NodeView = ???

  /**
    * Serializers for modifiers, to be provided by a concrete instantiation
    */
  override val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(PowBlock.ModifierTypeId -> PowBlockCompanion,
      Transaction.ModifierTypeId -> SimpleBoxTransactionSerializer)

  override val networkChunkSize: Int = settings.network.networkChunkSize

  private var aeneasSubscribers = mutable.Map[AeneasNodeViewHolder.NodeViewEvent.Value, Seq[ActorRef]]()

  protected def notifyAeneasSubscribers[E <: NodeViewHolderEvent](eventType: NodeViewEvent.Value, signal: E): Unit = {
    aeneasSubscribers.getOrElse(eventType, Seq()).foreach(_ ! signal)
  }

  /**
    * Handler for specific Aeneas signals.
    *
    * @see AeneasSynchronizer companion object for more signals.
    * @see Miner companion object for more signals.
    */
  protected def handleAeneasSubscribe: Receive = {
    case AeneasSubscribe(events) =>
      events.foreach { evt =>
        val current = aeneasSubscribers.getOrElse(evt, Seq())
        aeneasSubscribers.put(evt, current :+ sender())
      }
  }

  /**
    * Signal happens when application starts at first time
    * and full blockchain download should be requested.
    */
  protected def onRestoreMessage: Receive = {
    case NotifySubscribersOnRestore =>
      log.debug(
        s"""OnRestore message was received with
           |sync : ${synchronizerStatus.get()} &&
           |miner : ${minerStatus.get()}
            """.stripMargin)
      if (synchronizerStatus.get() && minerStatus.get()) {
        notifyAeneasSubscribers(NodeViewEvent.PreStartDownloadRequestEvent, PreStartDownloadRequestFull)
        notifyAeneasSubscribers(NodeViewEvent.StopMining, StopMining)
        if (history().height <= 0)
          updateNodeView(Option(AeneasHistory(nodeView._1.storage, nodeView._1.validators, nodeView._1.settings, downloadProcess = true)), None, vault(), None)
        else updateNodeView(Option(AeneasHistory(nodeView._1.storage, nodeView._1.validators, nodeView._1.settings)), None, vault(), None)
        //notifyAeneasSubscribers(NodeViewEvent.UpdateHistory, ChangedHistory(history()))
        synchronizerStatus.compareAndSet(true, false)
        minerStatus.compareAndSet(true, false)
      }
  }

  def checkHistory(history: AeneasHistory): Option[ModifierId] =
    if (history.isEmpty) None
    else if (history.modifierById(history.genesis()).isDefined) Option(history.genesis())
    else {
      def loop(id: ModifierId, pid: ModifierId): Option[ModifierId] = history.modifierById(id) match {
        case Some(b) => loop(b.parentId, id)
        case None => Some(pid)
      }

      loop(history.bestBlock().id, history.bestBlock().id)
    }

  protected def onFailedRecalculation: Receive = {
    case FailedHistory(id) =>
      log.debug(s"history corrupted on recalc balance with id = $id")
      notifyAeneasSubscribers(NodeViewEvent.PreStartDownloadRequestEvent, PreStartDownloadRequest(id))
  }

  protected def runAsyncHistoryCheck(reader: AeneasHistory): Future[Unit] =
    Future {
      log.debug("Start history checking")
      checkHistory(reader) match {
        case Some(id) =>
          log.debug(s"Check history result: $id")
          if (id.deep == reader.genesis().deep) {
            updateNodeView(Option(AeneasHistory(reader.storage, reader.validators, reader.settings)), None, vault(), None)
            //nodeView._1.downloadProcess.compareAndSet(true, false)
            prestartDownloadEnded.getAndSet(true)
            log.debug(s"Vault value: ${nodeView._3}")
            if (nodeView._3.nonEmpty && logged.get()) notifyAeneasSubscribers(NodeViewEvent.StartMining, StartMining) // check Vault
          } else {
            log.debug(s"History corrupted")
            notifyAeneasSubscribers(NodeViewEvent.PreStartDownloadRequestEvent, PreStartDownloadRequest(id))
          }
        case _ =>
          log.error("history problem")
          self ! NotifySubscribersOnRestore
      }
    }

  protected def onDownloadEnded: Receive = {
    case DownloadEnded(hisReader) =>
      log.debug("Download process was ended ")
      hisReader match {
        case Some(reader) =>
          runAsyncHistoryCheck(reader)
        case None =>
          self ! NotifySubscribersOnRestore
      }
  }

  protected def switchToMine: Receive = {
    case SwitchToMine() =>
      log.debug(s"prestartDownloadEnded: ${prestartDownloadEnded.get()}, logged: ${logged.get()}, wallet non emprty : ${nodeView._3.nonEmpty}")
      if (prestartDownloadEnded.get() && logged.get() && nodeView._3.nonEmpty)
        notifyAeneasSubscribers(NodeViewEvent.StartMining, StartMining)
  }

  protected def onLoggedIn: Receive = {
    case LoggedIn(seed) =>
      Base58.decode(seed) match {
        case Success(seedBytes) =>
          logged.getAndSet(true)
          val newVault = AeneasWallet.readOrGenerate(history(), settings, ByteStr(seedBytes), 1)
          updateNodeView(None, None, Option(newVault), None)
          log.debug(
            s"""set new vault value: ${nodeView._3}
               |prestartDownloadEnded.get() : ${prestartDownloadEnded.get()}
                  """.stripMargin)
          if (prestartDownloadEnded.get()) notifyAeneasSubscribers(NodeViewEvent.StartMining, StartMining)
        case _ =>
      }
  }

  protected def onLogOut: Receive = {
    case Logout(_) =>
      logged.getAndSet(false)
      updateNodeView(None, None, None, None)
      notifyAeneasSubscribers(NodeViewEvent.StopMining, StopMining)
  }

  /**
    * Signal is sent when synchronizer actor is alive.
    * It happens when application has first-time launch
    * and full blockchain download should be requested.
    */
  protected def onSynchronizerAlive: Receive = {
    case SynchronizerAlive =>
      synchronizerStatus.compareAndSet(false, true)
      if (!minerActivation.get() || !minerStatus.get())
        self ! NotifySubscribersOnRestore
      log.debug(s"changed history onSynAlive send:${ChangedHistory(history())}")
      notifyAeneasSubscribers(NodeViewEvent.UpdateHistory, ChangedHistory(history()))
  }

  /**
    * Signal is sent when miner actor is alive.
    * It happens when application has first-time launch
    * and full blockchain download should be requested.
    */
  protected def onMinerAlive: Receive = {
    case MinerAlive =>
      log.debug("AeneasViewHolder : Miner is alive")
      minerStatus.compareAndSet(false, true)
      if (minerActivation.get()) {
        notifyAeneasSubscribers(NodeViewEvent.StartMining, StartMining)
      }
      if (!synchronizerStatus.get())
        self ! NotifySubscribersOnRestore
  }

  override protected def getCurrentInfo: Receive = {
    case GetDataFromCurrentView(f) =>
      log.debug(s"handleAeneasSubscribe :$f")
      sender() ! f(CurrentView(history(), minimalState(), vault(), memoryPool()))
  }

  def processRemoteBestBlockModifiers: Receive = {
    case bb@RemoteBestBlockModifier(pb: PowBlock) =>
      val currentBestBlock = history().bestBlock()
      if (bb.pmod.parentId.deep == currentBestBlock.id.deep) {
        clientInformatorRef.foreach(_ ! bb.pmod)
        log.info(s"Got remote best modifier ${bb.pmod.encodedId} of type ${bb.pmod.modifierTypeId}")
        pmodModify(pb)
        notifyAeneasSubscribers(NodeViewEvent.UpdateHistory, ChangedHistory(history()))
        log.debug(s"nodeView._3.nonEmpty: ${nodeView._3}, ${nodeView._3.nonEmpty}")
        Future.successful {
          checkHistory(history()) match {
            case Some(id) =>
              log.info(s"Check history for new remote best block : ${Base58.encode(id)}")
              if (id.deep != history().genesis().deep) {
                log.warn(s"History corrupted!")
                notifyAeneasSubscribers(NodeViewEvent.PreStartDownloadRequestEvent, PreStartDownloadRequest(id))
              }
            case _ =>
              log.error("History has an unknown problem")
              self ! NotifySubscribersOnRestore
          }
        }
        if (nodeView._3.nonEmpty && logged.get()) notifyAeneasSubscribers(NodeViewEvent.StartMining, StartMining)
      } else {
        notifyAeneasSubscribers(NodeViewEvent.PreStartDownloadRequestEvent, PreStartDownloadRequestFull)
      }
  }

  def getBalance: Receive = {
    case GetWallet => recalculateBalance
      log.debug("GET WALLET")
      sender() ! nodeView._3
  }

//  def getPubWalletReceive = {
//    case FindBlockId(id: Int) =>
//    case FindBlockHash(hash: String) =>
//    case CountBalance(pubKeySet: PublicKey25519Proposition) =>
//    case CountMined(pubKeySet: PublicKey25519Proposition) =>
//    case HistoryTransaction(pubKeySet: PublicKey25519Proposition) =>
//    case FindTransaction(idHash: String) =>
//  }

  protected def recalculateBalance(pubKeySet: PublicKey25519Proposition): Long = {
    nodeView._3 match {
      case Some(wallet) =>
        wallet.fullCheckoutInputsOutputs(Set(pubKeySet)) match {
          case Right(res) =>
            updateNodeView(Some(history()), Some(minimalState()), Some(wallet), Some(memoryPool()))
            res.available
          case Left(id) =>
            self ! FailedHistory(id)
            0L
        }
      case _ => log.debug("recalc failed with None wallet")
        0L
    }
  }

  protected def recalculateBalance(): Unit = {
    nodeView._3 match {
      case Some(wallet) =>
        wallet.fullCheckoutInputsOutputs() match {
          case Right(_) =>
            updateNodeView(Some(history()), Some(minimalState()), Some(wallet), Some(memoryPool()))
          case Left(id) =>
            self ! FailedHistory(id)
        }
        log.debug(s"recalc balance value:${wallet.balance()}")

      case _ => log.debug("recalc failed with None wallet")
    }
  }

  protected def newBlockToBalance(pb: PowBlock): Unit = {
    nodeView._3 match {
      case Some(wallet) =>
        log.debug(s"wallet balance: ${wallet.balance()}")
        wallet.newBlockToBalance(pb)
        updateNodeView(Some(history()), Some(minimalState()), Some(wallet), Some(memoryPool()))
      case _ => log.debug("recalc balance failed with None wallet")
    }
  }

  protected def processRemoteGenerateTransaction: Receive = {
    case ModifiersFromRemote(remote, modifierTypeId, remoteObjects) =>
      modifierSerializers.get(modifierTypeId) foreach { companion =>
        remoteObjects.flatMap(r => companion.parseBytes(r).toOption).foreach {
          case tx: SimpleBoxTransaction@unchecked if tx.modifierTypeId == Transaction.ModifierTypeId =>
            txModify(tx)
        }
      }
  }

  protected def processLocallyGenerateTransaction: Receive = {
    case lt: LocallyGeneratedTransaction[P, TX] =>
      log.info(s"Got locally generated transaction ${lt.tx.encodedId} with value ${lt.tx.to.head._2}.")
      txModify(lt.tx)
  }

  private def addFeeTransaction(tx: SimpleBoxTransaction): Unit = {
    memoryPool().put(tx) match {
      case Success(newPool) =>
        log.debug(s"Fee transaction ${tx.encodedId} added to the memory pool")
        vault() match {
          case Some(vault) =>
            updateNodeView(updatedVault = Option(vault), updatedMempool = Some(newPool))
            notifySubscribers(EventType.SuccessfulTransaction, SuccessfulTransaction[P, TX](tx))
          case _ =>
            log.debug("vault is empty; may be user isn't logged in")
        }
      case Failure(e) =>
        notifySubscribers(EventType.FailedTransaction, FailedTransaction[P, TX](tx, e))
    }
  }

  override protected def txModify(tx: SimpleBoxTransaction): Unit = {
    memoryPool().put(tx) match {
      case Success(newPool) =>
        log.debug(s"Unconfirmed transaction ${tx.encodedId} added to the memory pool")
        vault() match {
          case Some(x) =>
            x.scanOffchain(tx)
            updateNodeView(updatedVault = Option(x), updatedMempool = Some(newPool))
            notifySubscribers(EventType.SuccessfulTransaction, SuccessfulTransaction[P, TX](tx))
          case _ =>
            log.debug("Vault is empty.")
        }
      case Failure(e) =>
        notifySubscribers(EventType.FailedTransaction, FailedTransaction[P, TX](tx, e))
    }
  }

  override def receive: Receive =
    onSynchronizerAlive orElse
      onMinerAlive orElse
      handleAeneasSubscribe orElse
      onRestoreMessage orElse
      getCurrentInfo orElse
      onDownloadEnded orElse
      processRemoteBestBlockModifiers orElse
      processLocallyGenerateTransaction orElse
      processRemoteGenerateTransaction orElse
      processRemoteModifiers orElse
      super.processRemoteModifiers orElse
      onFailedRecalculation orElse
      switchToMine orElse
      getBalance orElse
      onLoggedIn orElse
      onLogOut orElse
      super.processLocallyGeneratedModifiers orElse
      super.handleSubscribe orElse
      super.compareViews orElse
      super.getNodeViewChanges orElse {
      case UiInformatorSubscribe =>
        if (clientInformatorRef.isEmpty)
          clientInformatorRef = Option(sender())
      case a: Any => log.error("Strange input: " + a)
    }

  /**
    * The main data structure a node software is taking care about, a node view consists
    * of four elements to be updated atomically: history (log of persistent modifiers),
    * state (result of log's modifiers application to pre-historical(genesis) state,
    * user-specific information stored in vault (it could be e.g. a wallet), and a memory pool.
    */
  private val nodeViewState = Option.apply(new AtomicReference(restoreState().getOrElse(updateChainState().get)))

  override protected def updateNodeViewState(newNodeView: NodeView): Unit = {
    log.debug(s"updateNodeViewState") //:$newNodeView, nodeViewState:${if (nodeViewState.isDefined)nodeViewState.get.get()}")
    nodeViewState match {
      case Some(nodeView) =>
        log.debug(s"balance : ${if (nodeView.get()._3.nonEmpty) nodeView.get()._3.get.balance().available else 0}")
        nodeView.set(newNodeView)
        log.debug(s"balance : ${if (nodeView.get()._3.nonEmpty) nodeView.get()._3.get.balance().available else ""}")
      case _ =>
        log.debug("update view state : unexpected error")
    }
  }

  override protected def nodeView: NodeView = {
    nodeViewState match {
      case Some(nodeView) =>
        nodeView.get()
      case None =>
        log.debug("nodeViewState is None ")
        new AtomicReference(restoreState().getOrElse(updateChainState().get)).get()
    }
  }

}

object AeneasNodeViewHolder {

  object NodeViewEvent extends Enumeration {
    // miner events
    val StartMining: NodeViewEvent.Value = Value(1)
    val StopMining: NodeViewEvent.Value = Value(2)

    // synchronizer events
    val PreStartDownloadRequestEvent: NodeViewEvent.Value = Value(3)
    val UpdateHistory: NodeViewEvent.Value = Value(4)
  }

  case class AeneasSubscribe(minerEvents: Seq[NodeViewEvent.Value])

  case object NotifySubscribersOnRestore

  case object ActivateMining

  case object GetWallet

}
