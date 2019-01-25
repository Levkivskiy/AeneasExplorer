package history

import block.PowBlock
import history.sync.{VerySimpleSyncInfo, VerySimpleSyncInfoSerializer}
import block.PowBlockCompanion.BeneficiaryAddress
import org.scalatest.{FunSuite, Matchers}
import scorex.core.ModifierId
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.hash.Digest32
import settings.AeneasSettings

/**
  * @author luger.
  *         Created on 06.02.18.
  */
class VerySimpleSyncInfoSerializerTest extends FunSuite with Matchers {
  test ("From Air SyncInfo Serialize") {
    val settings = AeneasSettings.read()
    val genesisAccount = PrivateKey25519Companion.generateKeys("genesisBlock".getBytes, 0)

    val block1 = new PowBlock(
      settings.miningSettings.GenesisParentId,
      System.currentTimeMillis(),
      1L,
      Digest32 @@ Array.fill(32) (0 : Byte),
      genesisAccount._2,
      BeneficiaryAddress @@ genesisAccount._2,      Seq.empty
    )

    val block2 = new PowBlock(
      ModifierId @@ block1.id,
      System.currentTimeMillis(),
      2,
      Digest32 @@ Array.fill(32) (0 : Byte),
      genesisAccount._2,
      BeneficiaryAddress @@ genesisAccount._2,      Seq.empty
    )

    val block3 = new PowBlock(
      ModifierId @@ block2.id,
      System.currentTimeMillis(),
      3,
      Digest32 @@ Array.fill(32) (0 : Byte),
      genesisAccount._2,
      BeneficiaryAddress @@ genesisAccount._2,      Seq.empty
    )

    val block4 = new PowBlock(
      ModifierId @@ block3.id,
      System.currentTimeMillis(),
      4,
      Digest32 @@ Array.fill(32) (0 : Byte),
      genesisAccount._2,
      BeneficiaryAddress @@ genesisAccount._2,
      Seq.empty
    )

    val blockSeq = Seq(block1, block2, block3, block4)

    val simpleSyncInfo : VerySimpleSyncInfo = VerySimpleSyncInfo(blockSeq.size, blockSeq.map(_.id), block1.id)

    val decoded = VerySimpleSyncInfoSerializer.toBytes(simpleSyncInfo)
    val preEncoded = VerySimpleSyncInfoSerializer.parseBytes(decoded)
    assert(preEncoded.isSuccess)
    val encoded = preEncoded.get
    simpleSyncInfo.lastBlocks.lengthCompare(preEncoded.get.lastBlocks.size) shouldBe 0
    assert(simpleSyncInfo.genesisBlock.deep equals preEncoded.get.genesisBlock.deep)
    assert(simpleSyncInfo.blockchainHeight == preEncoded.get.blockchainHeight)
    encoded.lastBlocks.zip(simpleSyncInfo.lastBlocks).foreach(el => assert(el._1.deep == el._2.deep))
  }
}
