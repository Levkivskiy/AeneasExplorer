package coreentities

import block.{PowBlock, PowBlockCompanion, PowBlockHeader}
import history.{AeneasHistory, TempDbHelper}
import history.storage.AeneasHistoryStorage
import io.iohk.iodb.LSMStore
import block.PowBlockCompanion.BeneficiaryAddress
import commons.SimpleBoxTransactionGenerator
import org.scalatest.{FunSuite, Matchers}
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.hash.Digest32
import settings.AeneasSettings
import wallet.AeneasWallet

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 01.05.18.
  */
class BlockSerializationTest extends FunSuite with Matchers {
   test("Transaction extraction test") {
      val settings = AeneasSettings.read()
      val testFile = TempDbHelper.mkdir
      val store = new LSMStore(testFile, maxJournalEntryCount = 200)
      val storage = new AeneasHistoryStorage(store, settings.miningSettings)
      var history = new AeneasHistory(storage, Seq(), settings.miningSettings)

      val generator = new SimpleBoxTransactionGenerator(AeneasWallet.readOrGenerate(history, settings.scorexSettings))
      val txPool = generator.syncGeneratingProcess(5).toSeq
      val genesisAccount = PrivateKey25519Companion.generateKeys("genesisBlock".getBytes, 0)

      val block = new PowBlock(
         settings.miningSettings.GenesisParentId,
         System.currentTimeMillis(),
         100 << 20,
         Digest32 @@ Array.fill(32) (1 : Byte),
         genesisAccount._2,
         BeneficiaryAddress @@ genesisAccount._2,
         txPool
      )
      val blockByteArray  = PowBlockCompanion.toBytes(block)
      val txBlockBytePart = blockByteArray.slice(PowBlockHeader.PowHeaderSize, blockByteArray.length-8)
      println(txBlockBytePart.length)

      val extractedTxPool = PowBlockCompanion.extractTransactions(
            blockByteArray.slice(PowBlockHeader.PowHeaderSize, blockByteArray.length-8), Seq(), 0l, 0)

      println(txPool.size  + " and " + extractedTxPool._1.size)
      txPool.size shouldBe extractedTxPool._1.size
      txPool.zip(extractedTxPool._1).foreach(pair => pair._1 shouldBe pair._2)
   }

   test("Non-empty block serialization") {
      val genesisAccount = PrivateKey25519Companion.generateKeys("genesisBlock".getBytes, 0)
      val settings = AeneasSettings.read()
      val testFile = TempDbHelper.mkdir
      val store = new LSMStore(testFile, maxJournalEntryCount = 200)
      val storage = new AeneasHistoryStorage(store, settings.miningSettings)
      var history = new AeneasHistory(storage, Seq(), settings.miningSettings)

      val generator = new SimpleBoxTransactionGenerator(AeneasWallet.readOrGenerate(history, settings.scorexSettings))

      val txPool = generator.syncGeneratingProcess(100).toSeq

      val block = new PowBlock(
         settings.miningSettings.GenesisParentId,
         System.currentTimeMillis(),
         100 << 20,
         Digest32 @@ Array.fill(32) (1 : Byte),
         genesisAccount._2,
         BeneficiaryAddress @@ genesisAccount._2,
         txPool
      )

      val serialized = PowBlockCompanion.toBytes(block)
      val deserialized = PowBlockCompanion.parseBytes(serialized)

      block shouldBe deserialized.get
   }

   test("Empty block serialization") {
      val genesisAccount = PrivateKey25519Companion.generateKeys("genesisBlock".getBytes, 0)
      val settings = AeneasSettings.read()
      val testFile = TempDbHelper.mkdir
      val store = new LSMStore(testFile, maxJournalEntryCount = 200)
      val storage = new AeneasHistoryStorage(store, settings.miningSettings)
      var history = new AeneasHistory(storage, Seq(), settings.miningSettings)

      val generator = new SimpleBoxTransactionGenerator(AeneasWallet.readOrGenerate(history, settings.scorexSettings))

      val block = new PowBlock(
         settings.miningSettings.GenesisParentId,
         System.currentTimeMillis(),
         100 << 20,
         Digest32 @@ Array.fill(32) (1 : Byte),
         genesisAccount._2,
         BeneficiaryAddress @@ genesisAccount._2,
         Seq()
      )

      val serialized = PowBlockCompanion.toBytes(block)
      val deserialized = PowBlockCompanion.parseBytes(serialized)

      block shouldBe deserialized.get
   }
}


