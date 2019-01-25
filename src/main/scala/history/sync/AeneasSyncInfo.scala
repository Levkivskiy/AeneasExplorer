package history.sync

import block.PowBlock
import com.google.common.primitives.Longs
import scorex.core.consensus.SyncInfo
import scorex.core.mainviews.NodeViewModifier
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId}

import scala.util.Try

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 19.01.18.
  *
  */
case class VerySimpleSyncInfo(blockchainHeight : Long,
                              lastBlocks: Seq[ModifierId],
                              genesisBlock: ModifierId) extends SyncInfo {

   require(lastBlocks.size <= VerySimpleSyncInfo.lastBlocksCount)
   override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = {
      Seq(lastBlocks.map(block => PowBlock.ModifierTypeId -> block)).flatten
   }

   override type M = VerySimpleSyncInfo

   override def serializer: Serializer[VerySimpleSyncInfo] = VerySimpleSyncInfoSerializer
}

object VerySimpleSyncInfo {
   val lastBlocksCount = 10
}

object VerySimpleSyncInfoSerializer extends Serializer[VerySimpleSyncInfo] {
   override def toBytes(obj: VerySimpleSyncInfo): Array[Byte] =
      Longs.toByteArray(obj.blockchainHeight) ++
      Array(obj.lastBlocks.size.toByte) ++
      obj.lastBlocks.foldLeft(Array[Byte]()) ((a, b) => a ++ b) ++
      obj.genesisBlock


   override def parseBytes(bytes: Array[Byte]): Try[VerySimpleSyncInfo] = Try {
      val blockchainHeight = Longs.fromByteArray(bytes.slice(0, 8))
      val lastPowBlockIdsSize = bytes.slice(8, 9).head//TODO check bytes is not empty

      val offset = 8 + 1 // Long blockchain size + Byte lastPowBlockIdsSize

      require(lastPowBlockIdsSize >= 0 && lastPowBlockIdsSize <= VerySimpleSyncInfo.lastBlocksCount)
      require(bytes.length == offset + lastPowBlockIdsSize * NodeViewModifier.ModifierIdSize + NodeViewModifier.ModifierIdSize)

      val lastBlockIds =
         bytes.slice(offset, offset + NodeViewModifier.ModifierIdSize * lastPowBlockIdsSize)
              .grouped(NodeViewModifier.ModifierIdSize).toSeq.map(id => ModifierId @@ id)

      val genesisBlock = bytes.slice(bytes.length - NodeViewModifier.ModifierIdSize, bytes.length)

      VerySimpleSyncInfo(blockchainHeight, lastBlockIds, ModifierId @@ genesisBlock)
   }
}

object VerySimpleSyncInfoMessageSpec extends SyncInfoMessageSpec[VerySimpleSyncInfo](VerySimpleSyncInfoSerializer.parseBytes)
