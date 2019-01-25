/*
 * Copyright 2018, Aeneas Platform.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package network

import akka.actor.{Actor, ActorRef}
import block.{AeneasBlock, PowBlock}
import history.AeneasHistory
import network.BlockchainDownloader.{DownloadEnded, Idle, SendBlockRequest}
import network.messagespec.{DownloadInvSpec, EndDownloadSpec, PoWBlockMessageSpec, PoWBlocksMessageSpec}
import scorex.core.consensus.History
import scorex.core.mainviews.NodeViewHolder
import scorex.core.mainviews.NodeViewHolder.ReceivableMessages.{GetNodeViewChanges, Subscribe}
import scorex.core.network.NetworkController.ReceivableMessages.{RegisterMessagesHandler, SendToNetwork}
import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, NodeViewHolderEvent}
import scorex.core.network.message.BasicMsgDataTypes.InvData
import scorex.core.network.message.{InvSpec, Message, MessageSpec}
import scorex.core.network.{ConnectedPeer, SendToPeer}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.ScorexLogging
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.util.encode.Base58

import scala.annotation.tailrec
import scala.util.Try
import AppenderOpts._
import AppenderInstances._

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 06.03.18.
  */
class BlockchainDownloader(networkControllerRef : ActorRef,
                           viewHolderRef : ActorRef,
                           networkSettings: NetworkSettings,
                           specs: Seq[MessageSpec[_]]) extends Actor with ScorexLogging {

   protected val invSpec = new DownloadInvSpec(networkSettings.maxInvObjects)
   protected val batchMessageSpec = new PoWBlocksMessageSpec
   protected val endDownloadSpec = new EndDownloadSpec

   protected var historyReaderOpt: Option[AeneasHistory] = None

   override def preStart(): Unit = {
      networkControllerRef ! RegisterMessagesHandler(specs, self)

      val vhEvents = Seq(NodeViewHolder.EventType.HistoryChanged)
      viewHolderRef ! Subscribe(vhEvents)
      viewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = false)
   }

   /**
     * * New peer action *
     * Handles `SendBlockRequest` entity and sends request to download batch of blocks,
     * where first block is block with `id` identificator.
     */
   def onDownloadBlocks : Receive = {
      case SendBlockRequest(typeId, id, remotePeer) =>
         log.debug(s" NEW PEER ACTION : Request to download portion of block with ${Base58.encode(id)} id from ${remotePeer.socketAddress} are forming")
         requestBlockToDownload(typeId, id, remotePeer)
   }

   /**
     * * New peer action *
     * Makes request to well-known `peer` to download batch of blocks,
     * where first block is block with `id` identificator.
     * @param typeId
     * @param modifierId
     * @param peer
     */
   def requestBlockToDownload(typeId: ModifierTypeId, modifierId: ModifierId, peer : ConnectedPeer): Unit = {
      val msg = Message(invSpec, Right(typeId -> Seq(modifierId)), None)
      log.debug(s"Receive request from ${peer.socketAddress} to download block with ${Base58.encode(modifierId)}")
      networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
      log.debug(s" NEW PEER ACTION : Request to download portion of block was sent.")

      // TODO: track delivery
   }

   /**
     * * Well-known peer action *
     * Handles download request message with concrete blocks on well-known `peer`.
     * If genesis block was downloaded, send Stop signal to downloading
     */
   def receiveRequestToDownload : Receive = {
      case DataFromPeer(spec, _data, remotePeer) if spec.messageCode == invSpec.messageCode =>
         _data match {
            case data : InvData@unchecked =>
               if (spec.messageCode == invSpec.messageCode) {
                  log.debug(s"Receive request from ${remotePeer.socketAddress} to download block with ${Base58.encode(data._2.head)}")
                  historyReaderOpt match {
                     case Some(reader) =>
                        val historyReader = reader.asInstanceOf[AeneasHistory]
                        log.debug(s"GENESIS : ${Base58.encode(data._2.head)}")
                        if (data._2.count(el => historyReader.genesis().deep == el.deep) == 1)
                           networkControllerRef ! SendToNetwork(Message(endDownloadSpec, Right("end"), None), SendToPeer(remotePeer))
                        else {
                           val lastBlockParentId = historyReader.modifierById(data._2.head)
                           lastBlockParentId match {
                              case Some(parentOfLastBlock) =>
                                 val acquiredBlocks = acquireBlocksFromBlockchain (parentOfLastBlock.id, Seq(), historyReader)
                                 val msg = Message (batchMessageSpec, Right (acquiredBlocks), None)
                                 log.debug (s"Download portion length is : ${acquiredBlocks.length}, will send it to ${remotePeer.socketAddress}")
                                 networkControllerRef ! SendToNetwork (msg, SendToPeer (remotePeer) )
                              case None =>
                           }
                        }
                     case _ => log.debug(s"Can`t read history")
                  }
               }
            case x =>
             log.debug(s"surprise:x = $x")
         }
   }

   def checkBlocksPortion(history:AeneasHistory, blocks: Seq[PowBlock]) :List[ModifierId] =
     if (history.height > 0){
       val hBlocksIds: Seq[ModifierId] = history.lastBlockIds(history.bestBlock(), BlockchainDownloader.maxBlockResponce)
       hBlocksIds.filter(x => blocks.exists(_.id.deep == x.deep)).toList
     }else List()

   /**
     * New peer action *
     * Receiving blocks from well-known peer and applying them to blockchain.
     * It also sends response to this peer to download MOAR blocks.
     */
   def receiveBlocksFromWellKnownPeer : Receive = {
      case DataFromPeer(spec, data : Seq[PowBlock]@unchecked, remotePeer) =>
         if (spec.messageCode == batchMessageSpec.messageCode) {
            log.debug(s"HistoryReader status : ${historyReaderOpt.getOrElse(None)}")
            require(historyReaderOpt.isDefined)
            val checkedBlock = checkBlocksPortion(historyReaderOpt.get, data.reverse)
            log.debug(s"checkedBlock:$checkedBlock")
            applyBlocksToBlockchain(data.reverse)
            require(historyReaderOpt.isDefined)
            require(historyReaderOpt.get.height > 0)
            // TODO : check if all blocks was applied to database.
            if (data.reverse.head.id.deep == historyReaderOpt.get.genesis().deep || checkedBlock.length > 1) {
               log.debug(s"Genesis or stop block was reached!")
               self ! Idle
            }
            else requestBlockToDownload(data.reverse.head.modifierTypeId, data.reverse.head.id, remotePeer)
         }
         else log.debug(s"Wrong message code coming with Seq[PoWBlock]")
   }

   def receiveStopSignal : Receive = {
      case DataFromPeer(spec, data, remotePeer) if spec.messageCode == endDownloadSpec.messageCode =>
         self ! Idle
         viewHolderRef ! DownloadEnded(historyReaderOpt)
   }

   def handleIdleSignal : Receive = {
      case Idle =>
         viewHolderRef ! DownloadEnded(historyReaderOpt)
   }

   def historyChanged : Receive = {
      case ChangedHistory(reader: AeneasHistory@unchecked) if reader.isInstanceOf[AeneasHistory] =>
         //TODO isInstanceOf & typeErasure??
         historyReaderOpt = Some(reader)
   }

   /**
     * * Well-known peer action
     * Storage of blocks getter for sending that portion to new peer.
     * @param acquiringId current block
     * @param acquiredBlocks collection of already acquired blocks for sending.
     * @param historyReader
     */
   @tailrec
   final def acquireBlocksFromBlockchain(acquiringId : ModifierId,
                                         acquiredBlocks : Seq[PowBlock],
                                         historyReader: AeneasHistory) : Seq[PowBlock] = {
      if (acquiredBlocks.lengthCompare(BlockchainDownloader.maxBlockResponce) == 0)
         acquiredBlocks
      else {
         historyReader.modifierById(acquiringId) match {
            case Some(b) =>
               b match {
                  case block : PowBlock =>
                     if (historyReader.storage.isGenesis(block)) acquiredBlocks :+ block
                     else acquireBlocksFromBlockchain(block.parentId, acquiredBlocks :+ block, historyReader)
                  case _ =>
                     acquiredBlocks
               }
            case _ =>
               acquiredBlocks
         }
      }
   }

   /**
     * Store sequence of new blocks to current blockchain.
     * Operation of first-time launched node.
     * @param blocks new blocks from original blockchain
     */
   final def applyBlocksToBlockchain(blocks : Seq[PowBlock]) : Unit = {
      historyReaderOpt match {
         case Some(reader) =>
            var historyReader = reader.asInstanceOf[AeneasHistory]
            historyReader = applyBlocks(blocks, historyReader)
            log.info(s"HistoryReader after applying : $historyReader")
            historyReaderOpt = Option(historyReader)
         case _ =>
      }
   }

   @tailrec
   private def applyBlocks(blocks: Seq[AeneasBlock], historyReader : AeneasHistory) : AeneasHistory = blocks match {
      case Nil =>
         historyReader
      case head +: tail =>
         log.debug(s"Apply blocks : number ${head.number}")
         // test shows that it always gets an object.
         val newHistoryReader = if (historyReader.storage.modifierById(head.id).isEmpty){
            (historyReader :+ head).get._1
         } else historyReader
         applyBlocks(tail, newHistoryReader)
   }

   // 2nxvYFQWipvnvuwS5dre6uanXwhuyX3QRB5YkSUmsiHU

   override def receive: Receive = {
      historyChanged orElse
         onDownloadBlocks orElse
         receiveBlocksFromWellKnownPeer orElse
         receiveRequestToDownload orElse
         receiveStopSignal orElse
         handleIdleSignal orElse {
         case a => log.debug(s"unhandled message: $a")
      }
   }
}

object BlockchainDownloader {

   val maxBlockResponce = 10

   sealed trait DownloaderEvent extends NodeViewHolderEvent

   /**
     * SendBlockRequest entity for next blocks batch.
     * Its happens when node was first-time launched.
     * It sends request to remote `peer` which is well-known.
     * @param modifierTypeId - size of `id` in bytes.
     * @param id of pow block
     * @param peer remote peer which
     */
   case class SendBlockRequest(modifierTypeId: ModifierTypeId, id : ModifierId, peer : ConnectedPeer) extends DownloaderEvent

   case object Idle extends DownloaderEvent

   case class DownloadEnded(hisReader : Option[AeneasHistory]) extends DownloaderEvent
   case class FailedHistory(id: ModifierId) extends DownloaderEvent
}

trait DownloaderHistoryAppender[B<:AeneasBlock, H<:History[B, _, _]]{
   def appendDownloaded (h:H, block:B):Try[(H, History.ProgressInfo[B])]
}


object AppenderOpts{
   implicit class Opts[B<:AeneasBlock, H<:AeneasHistory](h:H){
      def :+(b:B)(implicit appender:DownloaderHistoryAppender[AeneasBlock, AeneasHistory]) : Try[(AeneasHistory, History.ProgressInfo[AeneasBlock])] = appender.appendDownloaded(h, b)
   }
}

object AppenderInstances {
   implicit val appender:DownloaderHistoryAppender[AeneasBlock, AeneasHistory] = (h: AeneasHistory, block: AeneasBlock) => {
      val newHistory = AeneasHistory(h.storage, h.validators, h.settings, downloadProcess = true)
      newHistory.append(block)
   }
}
