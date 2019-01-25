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

package settings

import akka.actor.ActorRef
import block.{AeneasBlock, PowBlock}
import commons.SimpleBoxTransaction
import mining.Miner.{MineBlock, StartMining, StopMining}
import scorex.core.mainviews.NodeViewHolder
import scorex.core.mainviews.NodeViewHolder.ReceivableMessages.Subscribe
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{LocalInterface, ModifierId}

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 31.01.18.
  */

class SimpleLocalInterface (override val viewHolderRef: ActorRef,
                            powMinerRef: ActorRef,
                            minerSettings: SimpleMiningSettings)
  extends LocalInterface[PublicKey25519Proposition, SimpleBoxTransaction, AeneasBlock] {

   private var block = false

   override def preStart(): Unit = {
      val events = Seq(
         NodeViewHolder.EventType.SuccessfulTransaction,
         NodeViewHolder.EventType.FailedTransaction,

         NodeViewHolder.EventType.StartingPersistentModifierApplication,
         NodeViewHolder.EventType.SyntacticallyFailedPersistentModifier,
         NodeViewHolder.EventType.SemanticallyFailedPersistentModifier,
         NodeViewHolder.EventType.SuccessfulSyntacticallyValidModifier,
         NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier,

         NodeViewHolder.EventType.OpenSurfaceChanged,
         NodeViewHolder.EventType.StateChanged,
         NodeViewHolder.EventType.FailedRollback,

         NodeViewHolder.EventType.DownloadNeeded
      )
      viewHolderRef ! Subscribe(events)
   }

   override protected def onSuccessfulTransaction(tx: SimpleBoxTransaction): Unit = {}

   override protected def onFailedTransaction(tx: SimpleBoxTransaction): Unit = {}

   override protected def onStartingPersistentModifierApplication(pmod: AeneasBlock): Unit = {}

   override protected def onSyntacticallySuccessfulModification(mod: AeneasBlock): Unit = {
      if (!block) {
         mod match {
            case wb: PowBlock =>
               powMinerRef ! MineBlock
            case _ =>
         }
      }
   }

   override protected def onSyntacticallyFailedModification(mod: AeneasBlock): Unit = {}

   override protected def onSemanticallySuccessfulModification(mod: AeneasBlock): Unit = {
      if (!block) {
         mod match {
            case wb: PowBlock =>
               powMinerRef ! MineBlock
            case _ =>
         }
      }
   }

   override protected def onSemanticallyFailedModification(mod: AeneasBlock): Unit = {}

   override protected def onNewSurface(newSurface: Seq[ModifierId]): Unit = {}

   override protected def onRollbackFailed(): Unit = {
      log.error("Too deep rollback occurred!")
   }

   override protected def onNoBetterNeighbour(): Unit = {
      powMinerRef ! StartMining
      block = false
   }

   override protected def onBetterNeighbourAppeared(): Unit = {
      powMinerRef ! StopMining
      block = true
   }
}
