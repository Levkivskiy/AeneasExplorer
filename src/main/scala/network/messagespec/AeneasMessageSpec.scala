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

package network.messagespec

import block.{PowBlock, PowBlockCompanion}
import com.google.common.primitives.Ints
import org.bouncycastle.util.Strings
import scorex.core.mainviews.NodeViewModifier
import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.{InvSpec, MessageSpec}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 06.03.18.*
  */

/**
  * * Message specification for genesis and last `PowBlock`'s
  */
class PoWBlockMessageSpec extends MessageSpec[PowBlock] {
   override val messageCode: MessageCode = 102.byteValue()
   override val messageName: String = "PowBlock"

   override def toBytes(obj: PowBlock): Array[Byte] = PowBlockCompanion.toBytes(obj)

   override def parseBytes(bytes: Array[Byte]): Try[PowBlock] = PowBlockCompanion.parseBytes(bytes)
}

/**
  * * Message specification for best `PowBlock`'s( need another spec code )
  */
class BestPoWBlockMessageSpec extends MessageSpec[NodeViewModifier] {
   override val messageCode: MessageCode = 103.byteValue()
   override val messageName: String = "BestPowBlock"

   override def toBytes(obj: NodeViewModifier): Array[Byte] = obj match {
      case o:PowBlock =>PowBlockCompanion.toBytes(o)
      case _ =>Array()
   }

   override def parseBytes(bytes: Array[Byte]): Try[NodeViewModifier] = PowBlockCompanion.parseBytes(bytes)
}

class ConsensusBestBlockSpec extends BestPoWBlockMessageSpec {
  override val messageCode:MessageCode = 104.byteValue()
}

class ConsensusBestBlockNotFoundSpec extends MessageSpec[Unit] {
  override val messageCode: MessageCode = 105: Byte

  override val messageName: String = "Best block not found message"

  override def parseBytes(bytes: Array[Byte]): Try[Unit] =
    Try(require(bytes.isEmpty, "Non-empty data for Best block not found"))

  override def toBytes(data: Unit): Array[Byte] = Array()
}

/**
  * * Message specification for sequence of `PowBlock`
  */
class PoWBlocksMessageSpec extends MessageSpec[Seq[PowBlock]] {
   override val messageCode: MessageCode = 101.byteValue()
   override val messageName: String = "PowBlocks"

   override def toBytes(obj: Seq[PowBlock]): Array[MessageCode] = {
      obj.foldLeft(Array[Byte]()) { (acc, el) =>
         acc ++ Ints.toByteArray(PowBlockCompanion.toBytes(el).length) ++ PowBlockCompanion.toBytes(el)
      }
   }

   override def parseBytes(bytes: Array[MessageCode]): Try[Seq[PowBlock]] = Try {
      val blocks = ArrayBuffer[PowBlock]()
      var offset = 0
      while (offset < bytes.length) {
         val nextBlockSize = Ints.fromByteArray(bytes.slice(offset, offset + 4))
         offset = offset + 4
         blocks.append(PowBlockCompanion.parseBytes(bytes.slice(offset, offset + nextBlockSize)).get)
         offset = offset + nextBlockSize
      }
      blocks.toSeq
   }
}


class FullBlockChainRequestSpec extends MessageSpec[Unit] {
   override val messageCode: MessageCode = 99.byteValue()
   override val messageName: String = "Blockchain"

   override def toBytes(obj: Unit): Array[MessageCode] = Array()

   override def parseBytes(bytes: Array[MessageCode]): Try[Unit] =
      Try(require(bytes.isEmpty, "Non-empty data for Blockchain Download Signal"))
}

class DownloadInvSpec (maxInvObjects : Int) extends InvSpec(maxInvObjects) {
   override val messageCode: MessageCode = 56.byteValue()
   override val messageName: String = "DownloadInv"
}

class EndDownloadSpec extends MessageSpec[String] {
   override val messageCode: MessageCode = 199.byteValue()
   override val messageName: String = "EndDownload"

   override def toBytes(obj: String): Array[MessageCode] = obj.getBytes

   override def parseBytes(bytes: Array[MessageCode]): Try[String] = Try { Strings.fromByteArray(bytes) }
}

