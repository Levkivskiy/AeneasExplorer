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

package history

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean

import block.{AeneasBlock, PowBlock}
import history.storage.AeneasHistoryStorage
import history.sync.VerySimpleSyncInfo
import io.iohk.iodb.LSMStore
import scorex.core.block.BlockValidator
import scorex.core.consensus.History._
import scorex.core.consensus.{Absent, History, ModifierSemanticValidity, Valid}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.ScorexLogging
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.util.encode.Base58
import settings.SimpleMiningSettings
import validators.DifficultyValidator

import scala.annotation.tailrec
import scala.util.{Failure, Try}

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 22.01.18.
  */
case class AeneasHistory(storage: AeneasHistoryStorage,
                    validators : Seq[BlockValidator[AeneasBlock]],
                    settings: SimpleMiningSettings, downloadProcess:Boolean = false)
  extends History[AeneasBlock, VerySimpleSyncInfo, AeneasHistory] with ScorexLogging {

   override type NVCT = AeneasHistory
   val height = storage.height
   var lastBlock : Option[PowBlock] = None

  def heightOf (id:ModifierId) = storage.heightOf(id)

  def getAll () = storage.getAll()
   def genesis() : ModifierId = {
      storage.getGenesis() match {
         case Some(wrapper) =>
            ModifierId @@ wrapper.data
         case _ => ModifierId @@ Array.emptyByteArray
      }
   }

   def bestBlock() : PowBlock = storage.bestBlock

   /**
     * @return append modifier to history
     */
   override def append(block: AeneasBlock): Try[(AeneasHistory, History.ProgressInfo[AeneasBlock])] = Try {
      log.info(s"Trying to append block #${block.number}, hash : ${Base58.encode(block.id)} to history")
      if (height > 0) log.info(s"Blocks time difference : ${block.timestamp - bestBlock().timestamp}")
      validators.map(_.validate(block)).foreach {
         case Failure(e) =>
            log.warn(s"Failed to validate block ${Base58.encode(block.id)}")
            throw e
         case _ =>
      }
      val progressInfo: ProgressInfo[AeneasBlock] =
         if (storage.isGenesis(block)) {
            storage.storeGenesis(block)
            lastBlock = storage.update(block, None, isBest = !downloadProcess)
            log.info(s"Genesis History.append postappend length : ${storage.height}; bestPowId : ${Base58.encode(storage.bestPowId)}")
            ProgressInfo(None, Seq(), Some(block), Seq())
         } else if (downloadProcess) {
            log.debug(s"Download process: block number : ${block.number + 1}, height : ${storage.height}")
            if (block.number+1 > storage.height)
               lastBlock = storage.update(block, None, isBest = true)
            else storage.update(block, None, isBest = false)

            log.info(s"History unsafe append length : ${storage.height}; bestPowId : ${Base58.encode(storage.bestPowId)}")
            ProgressInfo(None, Seq(), Some(block), Seq())
         } else {
            storage.heightOf(block.parentId) match {
               case Some(parentHeight) =>
                  val best = storage.height == storage.parentHeight(b = block)
                  lastBlock = storage.update(block, None, best)
                  val mod : ProgressInfo[AeneasBlock] = {
                     if (best && block.id.deep == storage.bestPowId.deep) {
                        log.info(s"New block incoming : ${Base58.encode(block.id)}")
                        ProgressInfo(None, Seq(), Some(block), Seq())
                     } else {
                        ProgressInfo(None, Seq(), None, Seq())
                     }
                  }
                  log.debug(s"Regular History.append postappend length : ${storage.height}")
                  mod
               case None =>
                  log.info(s"No parent block ${Base58.encode(block.parentId)}")
                  ProgressInfo(None, Seq(), None, Seq())
            }
         }
      (new AeneasHistory(storage, validators, settings, downloadProcess), progressInfo)
   }

   /**
     * Is there's no history, even genesis block
     */
   override def isEmpty: Boolean = height <= 0

   /**
     * Return modifier of type PM with id == modifierId
     *
     * @param modifierId - modifier id to get from history
     * @return
     */
   override def modifierById(modifierId: ModifierId): Option[AeneasBlock] = storage.modifierById(modifierId)

   /**
     * Return semantic validity status of modifier with id == modifierId
     *
     * @param modifierId - modifier id to check
     * @return
     */
   override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity = {
      modifierById(modifierId).map { _ =>
         Valid
      }.getOrElse(Absent)
   }

   /**
     * Report that modifier is valid from other nodeViewHolder components point of view
     *
     */
   //TODO: to know more about semantic validity
   override def reportSemanticValidity(modifier: AeneasBlock, valid: Boolean, lastApplied: ModifierId) :
   (AeneasHistory, History.ProgressInfo[AeneasBlock]) = {
      this -> History.ProgressInfo(None, Seq(), None, Seq())
   }

   /**
     * @return last block if a history is linear, otherwise it returns last blocks from a blocktree etc
     */
   override def openSurfaceIds(): Seq[ModifierId] = {
      if (isEmpty) Seq(settings.GenesisParentId)
      else Seq(storage.bestPowId)
   }

   def continuationsIds(from: Seq[(ModifierTypeId, ModifierId)], size: Int): Option[ModifierIds] = {
      def inList(m: AeneasBlock): Boolean = idInList(m.id) || storage.isGenesis(m)

      def idInList(id: ModifierId): Boolean = from.exists(f => f._2.deep == id.deep)

      log.info(s"History.continuationIds 'from' size : ${from.size}.")

      //Look without limit for case difference between nodes is bigger then size
      chainBack(storage.bestBlock, inList) match {
         case Some(chain) if chain.exists(id => idInList(id._2)) =>
            log.info(s"Other chain size is ${chain.size}, applied")
            Some(chain.take(size))
         case Some(chain) =>
            log.warn(s"Found chain without ids from remote, it's size is : ${chain.size}")
            var done = false
            None
         case _ => None
      }
   }

   def lastBlockIds(startBlock: AeneasBlock, count: Int): Seq[ModifierId] = {
      chainBack(startBlock, storage.isGenesis, count - 1).get.map(_._2)
   }

   /**
     * Ids of modifiers, that node with info should download and apply to synchronize
     */

   override def continuationIds(info: VerySimpleSyncInfo, size: Int): Option[ModifierIds] = {
      continuationsIds(info.startingPoints, size)
   }

   def parentBlock(m: AeneasBlock): Option[AeneasBlock] = modifierById(m.parentId)

   @tailrec
   private def chainBack(m: AeneasBlock,
                         until: AeneasBlock => Boolean,
                         limit: Int = Int.MaxValue,
                         acc: Seq[(ModifierTypeId, ModifierId)] = Seq()): Option[Seq[(ModifierTypeId, ModifierId)]] = {
      val sum: Seq[(ModifierTypeId, ModifierId)] = (PowBlock.ModifierTypeId -> m.id) +: acc

      if (limit <= 0 || until(m)) {
         Some(sum)
      } else {
         parentBlock(m) match {
            case Some(parent) => chainBack(parent, until, limit - 1, sum)
            case _ =>
               log.warn(s"Parent block for ${Base58.encode(m.id)} not found ")
               None
         }
      }
   }
   /**
     * Information about our node synchronization status. Other node should be able to compare it's view with ours by
     * this syncInfo message and calculate modifiers missed by our node.
     *
     * @return
     */
   override def syncInfo: VerySimpleSyncInfo =
      VerySimpleSyncInfo(storage.height, lastBlocks(VerySimpleSyncInfo.lastBlocksCount, storage.bestBlock).map(_.id), genesis())

   /**
     * Whether another's node syncinfo shows that another node is ahead or behind ours
     *
     * @param other other's node sync info
     * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
     */
   override def compare(other: VerySimpleSyncInfo): HistoryComparisonResult = {
      if (other.lastBlocks.isEmpty)
         Younger
      else{
         log.debug("History : Comparing begins!")

         val compareSize = syncInfo.lastBlocks.zipAll(other.lastBlocks, Array.empty[Byte], Array.empty[Byte]).count(el => el._1.deep != el._2.deep)
         if (compareSize == 0)
            Equal
         else findComparisonResultInSyncInfo(syncInfo, other)
      }
   }

   /**
     * Method tries to define chain's age comparison.
     *
     * @param currentSyncInfo active head of current node's blockchain.
     * @param otherSyncInfo   active head of other node's blockchain.
     * @return Younger status, if current chain more developed.
     * Older, if other chain is more developed.
     */
   private def findComparisonResultInSyncInfo(currentSyncInfo : VerySimpleSyncInfo, otherSyncInfo : VerySimpleSyncInfo) : HistoryComparisonResult = {
      if (otherSyncInfo.genesisBlock.deep != genesis().deep)
         Younger
      else{
         val currentBlocks = currentSyncInfo.lastBlocks.reverse
         val otherBlocks = otherSyncInfo.lastBlocks.reverse

         log.debug(s"currentSync:${currentSyncInfo.lastBlocks.map(Base58.encode(_))} , other: ${otherSyncInfo.lastBlocks.toSeq.map(Base58.encode(_))}")
         val firstCurrentBlockOpt = currentBlocks.headOption
         val firstOtherBlockOpt = otherBlocks.headOption
         val s = for {
            firstCurrentBlock <- firstCurrentBlockOpt
            firstOtherBlock   <- firstOtherBlockOpt
            res               <-Option {
               if (firstCurrentBlock.deep != firstOtherBlock.deep) { // this condition is unreachable, but check it.

                  if (otherBlocks.tail.exists(el => el.deep == firstCurrentBlock.deep))
                     Older
                  else if (currentBlocks.tail.exists(el => el.deep == firstOtherBlock.deep))
                     Younger

                  else Nonsense
               }
               else Nonsense
            }
         }yield res
         s.getOrElse(Nonsense)
      }
   }

   /**
     * Take `count` blocks from history.
     * @param count
     * @param startBlock specialize first block.
     * @return `count` blocks after `startBlock` in increasing order by age (the oldest block is head of Seq).
     */
   def lastBlocks(count: Int, startBlock: PowBlock): Seq[PowBlock] = if (isEmpty) {
      Seq()
   } else {
      @tailrec
      def loop(b: PowBlock, acc: Seq[PowBlock] = Seq()): Seq[PowBlock] = if (acc.length >= count) {
         acc
      } else {
         modifierById(b.parentId) match {
            case Some(parent: PowBlock) => loop(parent, b +: acc)
            case _ => b +: acc
         }
      }

      loop(startBlock)
   }


   def averageDelay(blockId: ModifierId, blockNum: Int): Try[Long] = Try {
      val block = modifierById(blockId).get
      (block.timestamp - parentBlock(block).get.timestamp) / blockNum
   }
}

object AeneasHistory extends ScorexLogging {

   def emptyHistory(minerSettings: SimpleMiningSettings) = {
      new AeneasHistory(null, Seq(), minerSettings)
   }

   def readOrGenerate(settings: ScorexSettings, minerSettings: SimpleMiningSettings): AeneasHistory = {
      readOrGenerate(settings.dataDir, settings.logDir, minerSettings)
   }

   def readOrGenerate(dataDir: File, logDir: File, settings: SimpleMiningSettings): AeneasHistory = {
      log.info(s"AeneasHistory : generation begins at ${dataDir.getAbsolutePath}")
      val iFile = new File(s"${dataDir.getAbsolutePath}/blocks")
      iFile.mkdirs()
      val blockStorage = new LSMStore(iFile, maxJournalEntryCount = 10000)

      val storage = new AeneasHistoryStorage(blockStorage, settings)

      Runtime.getRuntime.addShutdownHook(new Thread() {
         override def run(): Unit = {
            log.info("Closing block storage...")
            blockStorage.close()
         }
      })
      log.debug(s"AeneasHistoryStorage height on generating state : ${storage.height}")

      val validators = Seq(new DifficultyValidator(settings, storage))

      new AeneasHistory(storage, validators, settings)
   }
}
