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

package history.storage

import block.{AeneasBlock, PowBlock, PowBlockCompanion}
import com.google.common.primitives.Longs
import commons.SimpleBoxTransaction
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.ModifierId
import scorex.core.block.Block
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.util.encode.Base58
import scorex.crypto.hash.Sha256
import settings.SimpleMiningSettings

import scala.util.Failure

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 22.01.18.
  */
class AeneasHistoryStorage(storage : LSMStore, settings : SimpleMiningSettings) extends ScorexLogging {
   private val bestBlockIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-1: Byte))

   def bestPowId: ModifierId = storage.get(bestBlockIdKey)
                                      .map(d => ModifierId @@ d.data)
                                      .getOrElse(settings.GenesisParentId)
   def getAll ():Iterator[(ByteArrayWrapper, ByteArrayWrapper)] = storage.getAll()
   log.debug(s"bestPowId: ${Base58.encode(bestPowId)}")
   def height : Long = heightOf(bestPowId).getOrElse(0L)
   def bestBlock: PowBlock = {
      require(height > 0, "History is empty")
      modifierById(bestPowId).get.asInstanceOf[PowBlock]
   }

   /** Store genesis block to special cell for fast receiving.
     *
     * Note: magic constant array was used as a key,
     * because we can't use evident string like "genesis"
     * because of `32` bytes key size =(.
     * @param b genesis block
     */

   def storeGenesis(b : AeneasBlock): Unit = {
      val genesisKey = "genesisStoreKey"
      storage.update(ByteArrayWrapper(genesisKey.getBytes("UTF-16")),
                     Seq(),
                     Seq(ByteArrayWrapper(genesisKey.getBytes("UTF-16")) -> ByteArrayWrapper(b.id)))
   }

   /** Receiving wrapped genesis block from key-value storage */

   def getGenesis(): Option[ByteArrayWrapper] = {
      val genesisKey = "genesisStoreKey"
      storage.get(ByteArrayWrapper(genesisKey.getBytes("UTF-16")))
   }

   def update(b: AeneasBlock, diff: Option[(BigInt, Long)], isBest: Boolean) : Option[PowBlock] = {
      log.debug(s"History.update : Write new ${if (isBest) "the best" else ",but not the best"} block ${b.encodedId}")
      val typeByte = b match {
         case _: PowBlock =>
            PowBlock.ModifierTypeId
      }

      log.debug(s"number:${b.number}, id:${b.encodedId}")
      val blockH: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      //Seq(blockHeightKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentHeight(b) + 1)))
      Seq(blockHeightKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(b.number + 1l)))

      val blockDiff: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = diff.map { d =>
         Seq(blockDiffKey(b.id) -> ByteArrayWrapper(d._1.toByteArray),
              blockDiffKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(d._2)))
      }.getOrElse(Seq())

      val bestBlockSeq: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = b match {
         case powBlock: PowBlock if isBest =>
            Seq(bestBlockIdKey -> ByteArrayWrapper(powBlock.id))
         case _ => Seq()
      }

      if (!storage.versionIDExists(ByteArrayWrapper(b.id))) {
         storage.update(
            ByteArrayWrapper(b.id),
            Seq(),
            blockDiff ++ blockH ++ bestBlockSeq ++ Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(typeByte +: b.bytes)))

         val check = storage.lastVersionID.getOrElse(-1L)
         log.debug(s"History.storage bestId : ${bestBlock.encodedId}")
         Option(b.asInstanceOf[PowBlock])
      }
      else {
         log.debug(s"Block ${b.encodedId} already exists!")
         Option(bestBlock)
      }
   }

   def getPoWDifficulty(idOpt: Option[ModifierId]): BigInt = {
      idOpt match {
         case Some(id) if id.sameElements (settings.GenesisParentId) =>
            settings.initialDifficulty
         case Some(id) =>
            BigInt(storage.get(blockDiffKey(id)).get.data)
         case _ =>
            settings.initialDifficulty
      }
   }

   def parentHeight(b: AeneasBlock): Long = heightOf(parentId(b)).getOrElse(0L)

   def parentId(block: AeneasBlock): ModifierId = block match {
      case powBlock: PowBlock => powBlock.parentId
   }

   def modifierById(blockId: ModifierId): Option[AeneasBlock with
     Block[PublicKey25519Proposition, SimpleBoxTransaction]] = {
      storage.get(ByteArrayWrapper(blockId)).flatMap { bw =>
         val bytes = bw.data
         val mtypeId = bytes.head
         val parsed = mtypeId match {
            case t: Byte if t == PowBlock.ModifierTypeId =>
               PowBlockCompanion.parseBytes(bytes.tail)
         }
         parsed match {
            case Failure(e) => log.warn("Failed to parse bytes from bd", e)
            case _ =>
         }
         parsed.toOption
      }
   }

   def isGenesis(b: AeneasBlock): Boolean = b match {
         case block: PowBlock => block.parentId.deep == settings.GenesisParentId.deep
         case _ => false
   }

   private def blockDiffKey(blockId: Array[Byte]): ByteArrayWrapper =
      ByteArrayWrapper(Sha256(s"difficulties".getBytes ++ blockId))

   private def blockHeightKey(blockId: Array[Byte]): ByteArrayWrapper =
      ByteArrayWrapper(Sha256("height".getBytes ++ blockId))

   //noinspection ScalaStyle
   def heightOf(blockId: Array[Byte]): Option[Long] = {
      storage.get(blockHeightKey(blockId))
        .map(b => Longs.fromByteArray(b.data))

   }
}
