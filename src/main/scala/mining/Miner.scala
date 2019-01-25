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

package mining

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import block.PowBlockCompanion.{BeneficiaryAddress, BlockNumber}
import block.{AeneasBlock, PowBlock}
import commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool, Value}
import history.AeneasHistory
import history.storage.AeneasHistoryStorage
import scorex.core.LocallyGeneratedModifiersMessages.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.block.Block.Timestamp
import scorex.core.{MerkleHash, ModifierId, TxId}
import scorex.core.mainviews.NodeViewHolder.CurrentView
import scorex.core.mainviews.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.NodeViewHolderEvent
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.encode.Base58
import settings.{AeneasSettings, SimpleMiningSettings}
import state.SimpleMininalState
import viewholder.AeneasNodeViewHolder.{AeneasSubscribe, NodeViewEvent}
import wallet.AeneasWallet

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 30.01.18.
  */
class Miner(viewHolderRef: ActorRef,
            settings : AeneasSettings) extends Actor with ScorexLogging {

   import Miner._
   private var cancellableOpt: Option[Cancellable] = None
   private val mining = new AtomicBoolean(false)
   private val minersCount = new AtomicInteger(0)
   private implicit val currentViewTimer: FiniteDuration = 50.millisecond
   private implicit val timeoutView = new Timeout(currentViewTimer)
   private implicit val cryptographicHash = Blake2b256

   private val minHashLiterals = settings.miningSettings.minHashLiterals
   private val blockGenerationDuration = settings.miningSettings.blockGenDelay.toMillis

   // Best fitness block if can't find any better.
   var bestFitnessBlock : AtomicReference[PowBlock] = new AtomicReference[PowBlock]()

   private var currentUnconfirmed : Seq[SimpleBoxTransaction] = Seq()
   private val currentMemPool : AtomicReference[SimpleBoxTransactionMemPool] = new AtomicReference()

   override def preStart(): Unit = {
      viewHolderRef ! AeneasSubscribe(Seq(NodeViewEvent.StartMining, NodeViewEvent.StopMining))
      viewHolderRef ! MinerAlive
   }

   /**
     * Function - criteria for choosing 'best' block.
     * We define that block is better if it has more 'a' in the beginning.
     * @param hash - new block hash.
     * @param hits - count of 'a' literal at the hash string beginning.
     * @return hits.
     */
   @tailrec
   private def checkFitness(hash : String, hits : Int) : Int = {
      if (hash.head == 'a')
         checkFitness(hash.tail, hits + 1)
      else hits
   }

   /**
     * Update current mempool from nodeView.
     * It happens where mining process is active.
     * @param viewHolderRef
     * @return
     */
   private def updateMempool(viewHolderRef : ActorRef) : Try[SimpleBoxTransactionMemPool] = {
      val currentViewAwait = ask(viewHolderRef, GetDataFromCurrentView(applyMempool)).mapTo[SimpleBoxTransactionMemPool]
      Await.ready(currentViewAwait, currentViewTimer).value.get
      //applyServiceTx(viewHolderRef, Await.result(currentViewAwait, currentViewTimer))
   }

   private def mineBlock(blockAttempt : PowBlock,
                         activeMempool: SimpleBoxTransactionMemPool,
                         currentTime : Timestamp,
                         endTime : Timestamp) : PowBlock = {
      if (currentTime > endTime)
         return blockAttempt
      currentMemPool.set(activeMempool)

      if (currentTime % 5000 == 0) {
         log.info(s"Transaction pool size before update : ${currentMemPool.get.size}")
         updateMempool(viewHolderRef) match {
            case Success(memPool) =>
               log.info(s"Current time is $currentTime, time to mining end is ${endTime-currentTime} ms.")
               currentMemPool.set(memPool)
            case Failure(ex) =>
               log.error(s"Current time is $currentTime, transaction pool load failed;", ex)
         }
      }

      currentUnconfirmed = currentMemPool.get().getUnconfirmed()
      var hash : Digest32 = Digest32 @@ Array.fill(32) (1 : Byte)

      if (currentUnconfirmed.nonEmpty) {
         val tree: MerkleTree[MerkleHash] = MerkleTree.apply(LeafData @@ currentUnconfirmed.map(tx => tx.id))
         hash = tree.rootHash
      }

      val block = PowBlock(
         blockAttempt.parentId,
         System.currentTimeMillis(),
         Math.abs(Random.nextLong()),
         hash,
         blockAttempt.generatorProposition,
         blockAttempt.beneficiaryAddress,
         currentUnconfirmed
      )

      val encodedId = Base58.encode(block.id)
      val currentFitness = checkFitness(block.encodedId, 0)
      if (currentFitness > minHashLiterals - 1)
         bestFitnessBlock.set(block)

      if (mining.get()) {
         if (checkFitness(block.encodedId, 0) > minHashLiterals) {
            log.debug(s"Current block ${block.encodedId} is the best fit, leaving mining process.")
            return block
         } else if (endTime - currentTime < 100) {
             if (bestFitnessBlock.get() == null)
                return block
            else bestFitnessBlock.get()
         }
      }
      mineBlock(block, activeMempool, System.currentTimeMillis(), endTime)
   }

   /**
     * It fills mining mempool with specific transactions.
     * They are described here : @see
     * @see https://github.com/AeneasPlatform/Aeneas/issues/53
     * @param memPool
     * @return
     */
   // TODO: Implement it.
   private def fillMempool(memPool: SimpleBoxTransactionMemPool) = ???

   def prevBlockFeeGrabAttempt(parentBlock: PowBlock,
                               proposition: PublicKey25519Proposition) : Option[Seq[SimpleBoxTransaction]] = {

      val overallFee = parentBlock.transactions.foldLeft(0L)(_ + _.fee)
      if (overallFee <= 0L)
         return None

      @tailrec
      def loop(transactions: Seq[SimpleBoxTransaction],
               fees: Seq[SimpleBoxTransaction]) : Seq[SimpleBoxTransaction] = {
         log.info(s"Fee grabbing : ${fees.size} iteration. ${transactions.size} left.")
         if (transactions.isEmpty) {
            log.info(s"Finish transactions counting.")
            return fees
         }

         if (transactions.head.fee <= 0L) {
            loop(transactions.tail, fees)
         } else {
            val tx = SimpleBoxTransaction.apply(
               IndexedSeq(),
               IndexedSeq(proposition -> Value @@ transactions.head.fee, transactions.head.to.head._1 -> Value @@ -transactions.head.fee),
               IndexedSeq(),
               0,
               System.currentTimeMillis()
            )
//            val tx = SimpleBoxTransaction.createFee(
//               transactions.head.to.head._1,
//               proposition -> Value @@ transactions.head.fee
//            )
            loop(transactions.tail, fees :+ tx)
         }
      }
      Option(loop(parentBlock.transactions, Seq()))
   }

   def miningProcess(parentBlock: PowBlock,
                     difficulty: BigInt,
                     settings: SimpleMiningSettings,
                     proposition: PublicKey25519Proposition,
                     address: BeneficiaryAddress,
                     blockGenerationDelay: FiniteDuration,
                   ): Option[PowBlock] = {
      val nonce = Math.abs(Random.nextLong())
      val ts = System.currentTimeMillis()

      val currentViewAwait = ask(viewHolderRef, GetDataFromCurrentView(applyMempool)).mapTo[SimpleBoxTransactionMemPool]

      log.info("Mining process starts")
      // mempool update.
      currentMemPool.set(Await.result(currentViewAwait, currentViewTimer))
      val (pool, serviceTxsIds) = applyServiceTx(viewHolderRef, currentMemPool.get)

      // collect fee if previous block was mined by current node.
      if (parentBlock.generatorProposition.equals(proposition)) {
         prevBlockFeeGrabAttempt(parentBlock, proposition) match {
            case Some(feeTransaction) =>
               pool.put(feeTransaction)
            case None =>
              log.warn(s"Transactions weren't collected")
         }
      }
      currentMemPool.set(pool)
      currentUnconfirmed = currentMemPool.get.getUnconfirmed()
      log.info(s"Current unconfirmed transaction pool size : ${currentUnconfirmed.size}")

      var hash : Digest32 = Digest32 @@ Array.fill(32) (1 : Byte)

      if (currentUnconfirmed.nonEmpty) {
         val tree: MerkleTree[MerkleHash] = MerkleTree.apply(LeafData @@ currentUnconfirmed.map(tx => tx.id))
         hash = tree.rootHash
      }

      val blockAttempt = PowBlock(
         parentBlock.id,
         ts,
         nonce,
         hash,
         proposition,
         address,
         currentUnconfirmed
      )

      val b : PowBlock = mineBlock(blockAttempt,
         currentMemPool.get(),
         System.currentTimeMillis(),
         System.currentTimeMillis() + blockGenerationDuration
      )

      val foundBlock =
         if (b.correctWork(difficulty, settings)) {
            Some(b)
         } else {
            None
         }
      log.debug(s"Service Tx Size :${serviceTxsIds.size}, current MemPool Size : ${currentMemPool.get.size}")
      serviceTxsIds.foreach(tx => currentMemPool.get().removeTransaction(tx))
      foundBlock
   }

   //noinspection ScalaStyle
   override def receive: Receive = {
      case StartMining =>
         mining.compareAndSet(false, true)
         log.debug(s"Miner : Mining was ${if (!mining.get()) "not" else ""} enabled, miners:${minersCount.get()}")
         if (settings.miningSettings.blockGenDelay >= 1.minute) {
            log.info("Mining is disabled for blockGenerationDelay >= 1 minute")
         } else {
            if (mining.get() && minersCount.get() == 0) {
               minersCount.getAndSet(1)
               log.info("Mining should begins.")
               self ! MineBlock
            }
            else StartMining
         }

      case MineBlock =>
         if (mining.get()) {
            log.info("Mining of previous PoW block stopped")
            cancellableOpt.forall(_.cancel())
            log.debug(s"MineBlock : Cancellable count : ${cancellableOpt.size}")
            // TODO: See here!!
            context.system.scheduler.scheduleOnce(250.millis) {
               log.debug(s"MineBlock : is in scheduler state")
               if (cancellableOpt.forall(_.status.isCancelled) || cancellableOpt.isEmpty) {
                  log.debug(s"MineBlock : it sends required data")
                  val dataFromCurrentView = getRequiredData
                  viewHolderRef ! dataFromCurrentView
               } else self ! StartMining
            }
         }

      case Some (pmi: MiningInfo) =>
         if (!cancellableOpt.forall(_.status.isCancelled)) {
            log.warn("Trying to run miner when the old one is still running")
         } else {
            val difficulty = pmi.powDifficulty
            val bestPowBlock = pmi.bestPowBlock
            log.info(s"Starting new block mining for ${bestPowBlock.encodedId}")

            val pubkey = pmi.pubkey

            val p = Promise[Option[PowBlock]]()
            cancellableOpt = Some(Cancellable.run() { status =>
               Future {
                  var foundBlock: Option[PowBlock] = None
                  while (status.nonCancelled && foundBlock.isEmpty) {
                     foundBlock = miningProcess(bestPowBlock, difficulty, settings.miningSettings, pubkey, pmi.regularAddress, settings.miningSettings.blockGenDelay)
                     log.debug(s"New block status : ${if (foundBlock.isDefined) "mined" else "in process"}")
                  }
                  p.success(foundBlock)
                  log.info(s"New block : ${foundBlock.get.encodedId}")
               }
            })
            p.future.onComplete {
               case Success(toBlock) =>
                  toBlock.foreach { block =>
                     log.info(s"Locally generated PoW block: ${block.encodedId}")
                     self ! block
                  }
               case Failure (x) =>
                log.warn(s"Block mining failed : $x")
            }
         }

      case b: PowBlock =>
         cancellableOpt.foreach(_.cancel())
         //TODO move it to synchronizer
         implicit val currentViewDuration: FiniteDuration = 5.millisecond
         val currentViewData = GetDataFromCurrentView(applyHistory)
         val currentViewAwait = ask(viewHolderRef, currentViewData).mapTo[AeneasHistory]

         Await.ready(currentViewAwait, currentViewDuration).onComplete{
            case Success(history) =>
               viewHolderRef ! LocallyGeneratedModifier[AeneasBlock](b.copy(number = BlockNumber @@ history.storage.height))
               minersCount.getAndSet(0)
               log.debug(s"height before sending for comparing best blocks: ${history.storage.height}")
            case Failure(ex) =>
               log.error("getting history failed on : ", ex)
               minersCount.getAndSet(0)
         }
      case StopMining =>
         log.debug(s"Pre-stop miner state : ${mining.toString}")
         mining.set(false)
         minersCount.getAndSet(0)
         log.debug(s"Miner : Mining was disabled")

      case a: Any =>
         log.warn(s"Strange input: $a")
   }
}

object Miner extends App with ScorexLogging {
   def applyMempool(currentView: CurrentView[AeneasHistory,
      SimpleMininalState, AeneasWallet, SimpleBoxTransactionMemPool]) : SimpleBoxTransactionMemPool = {
      currentView.pool
   }

   def applyWallet(currentView: CurrentView[AeneasHistory,
      SimpleMininalState, Option[AeneasWallet], SimpleBoxTransactionMemPool]) : Option[AeneasWallet] = {
      currentView.vault
   }

   def applyHistory(currentView: CurrentView[AeneasHistory,
      SimpleMininalState, Option[AeneasWallet], SimpleBoxTransactionMemPool]) : AeneasHistory = {
      currentView.history
   }

   /**
     * Service transactions are applying to the mempool (issue-55).
     * 80 Ashes - mining reward,
     * 10 Ashes - public benefit fund reward.
     * 8  Ashes - master nodes support.
     * 2  Ashes - owner  support.
     * @param viewHolderRef
     * @param txPool
     * @return
     */
   def applyServiceTx(viewHolderRef : ActorRef, txPool: SimpleBoxTransactionMemPool) : (SimpleBoxTransactionMemPool, Seq[TxId]) = {
      implicit val currentViewDuration: FiniteDuration = 5.millisecond
      implicit val currentViewTimer: Timeout = new Timeout(currentViewDuration)
      val currenViewData = GetDataFromCurrentView(applyWallet)
      val currentViewAwait = ask(viewHolderRef, currenViewData).mapTo[Option[AeneasWallet]]

      val wallet = Await.result(currentViewAwait, currentViewDuration).get
      log.debug(s"wallet : $wallet")
      // unconditional trust to these addresses, ignores Try
      val fundAddress = PublicKey25519Proposition.validPubKey("Æx4PSzf7PUD4ME3ju6PR6PkotYWisaUsZEYw6SpKe2zNeNXdmExu").get
      val mastersRewardAddress = PublicKey25519Proposition.validPubKey("Æx4dDrkaU51umZ79wyyseYzvuVr8RVJYyENo9Cs7XQhc73FZdyZW").get
      val ownerSupportAddress = PublicKey25519Proposition.validPubKey("Æx3iR6oLazw3P9K5oFzRBHzayaQJVeWiHDPziJZU63b9ShoVi9iB").get

      // TODO: why doesn't work?
      // val sig = Signature25519(Signature @@ Base58.decode("reward").get)

      val serviceTx = IndexedSeq(
      // reward for the block    : 80 * 10^8 granoes.
         new SimpleBoxTransaction(IndexedSeq(), IndexedSeq(wallet.publicKeys.head -> Value @@ 8000000000L), IndexedSeq(), 0, System.currentTimeMillis()),
         // public benefit fund fee : 10 * 10^8 granoes.
         new SimpleBoxTransaction(IndexedSeq(), IndexedSeq(fundAddress -> Value @@ 1000000000L), IndexedSeq(), 0, System.currentTimeMillis()),
         // master nodes support fee: 8 * 10^8 granoes.
         new SimpleBoxTransaction(IndexedSeq(), IndexedSeq(mastersRewardAddress -> Value @@ 800000000L), IndexedSeq(), 0, System.currentTimeMillis()),
         // owners support fee      : 2 * 10^8 granoes.
         new SimpleBoxTransaction(IndexedSeq(), IndexedSeq(ownerSupportAddress -> Value @@ 200000000L), IndexedSeq(), 0, System.currentTimeMillis())
      )
      (txPool.put(serviceTx) match {
         case Success(pool) => pool
         case Failure(exception) =>
            log.warn(s"Exception : ${exception.getMessage}")
            txPool
      }, serviceTx.map(_.id))
   }

   sealed trait MinerEvent extends NodeViewHolderEvent

   case object MinerAlive extends NodeViewHolderEvent

   case object StartMining extends MinerEvent

   case object UiInformatorSubscribe

   case object StopMining extends MinerEvent

   case object MineBlock extends MinerEvent


   case class MiningInfo(powDifficulty: BigInt, bestPowBlock: PowBlock, pubkey: PublicKey25519Proposition, regularAddress:BeneficiaryAddress) extends MinerEvent

   def getRequiredData: GetDataFromCurrentView[AeneasHistory,
     SimpleMininalState,
     Option[AeneasWallet],
     SimpleBoxTransactionMemPool,
     Option[MiningInfo]] = {
      val f: CurrentView[AeneasHistory, SimpleMininalState, Option[AeneasWallet], SimpleBoxTransactionMemPool] => Option[MiningInfo] = {
         view: CurrentView[AeneasHistory, SimpleMininalState, Option[AeneasWallet], SimpleBoxTransactionMemPool] =>
           view.vault match {
              case Some(vault) =>
                 log.debug(s"Miner.requiredData : work begins")

                 val bestBlock = view.history.storage.bestBlock
                 val difficulty = view.history.storage.getPoWDifficulty(None)

                 val (pubkey, regularAddress) = if (vault.publicKeys.nonEmpty) {
                    (vault.publicKeys.head, BeneficiaryAddress @@ vault.secrets.toSeq.minBy(_.order).publicImage)
                 } else {
                    val secret = view.vault.get.generateNewSecret().publicKeys.head
                    (secret, BeneficiaryAddress @@ secret)
                 }

                 log.debug(s"miningInfo: ${MiningInfo(difficulty, bestBlock.copy(transactionPool = Seq.empty), pubkey, regularAddress)}")
                 Some (MiningInfo(difficulty, bestBlock, pubkey, regularAddress))
              case _ => None
           }
      }
      GetDataFromCurrentView[AeneasHistory,
        SimpleMininalState,
        Option[AeneasWallet],
        SimpleBoxTransactionMemPool,
        Option[MiningInfo]](f)
   }

}
