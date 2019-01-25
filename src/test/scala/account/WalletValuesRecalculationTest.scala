package account

import block.PowBlock
import commons.{SimpleBoxTransaction, Value}
import history.storage.AeneasHistoryStorage
import history.{AeneasHistory, TempDbHelper}
import io.iohk.iodb.LSMStore
import block.PowBlockCompanion.BeneficiaryAddress
import org.scalatest.{FunSuite, Matchers}
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.hash.Digest32
import settings.AeneasSettings
import wallet.AeneasWallet

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 10.05.18.
  */
class WalletValuesRecalculationTest extends FunSuite with Matchers {
   test("Initial wallet input calculation with inputs") {
      val settings = AeneasSettings.read()
      val testFile = TempDbHelper.mkdir

      val storage = new AeneasHistoryStorage (new LSMStore(testFile, maxJournalEntryCount = 100), settings.miningSettings)
      var history = new AeneasHistory(storage, Seq(), settings.miningSettings)

      val genesisAccount = PrivateKey25519Companion.generateKeys("genesisBlock".getBytes, 0)

      val wallet = AeneasWallet.readOrGenerate(history, settings.scorexSettings)
      wallet.generateNewSecret()

      require(wallet.publicKeys.nonEmpty)

      val tx10 = genesisAccount._2 -> Value @@ -10.toLong
      val tx20 = genesisAccount._2 -> Value @@ -20.toLong
      val tx30 = genesisAccount._2 -> Value @@ -30.toLong

      val txs1 = Seq(
         SimpleBoxTransaction.create(wallet, tx10, 1: Long, Seq()).get,
         SimpleBoxTransaction.create(wallet, tx20, 1: Long, Seq()).get,
         SimpleBoxTransaction.create(wallet, tx30, 1: Long, Seq()).get,
         SimpleBoxTransaction.create(wallet, tx20, 1: Long, Seq()).get
      )

      val txs2 = Seq(
         SimpleBoxTransaction.create(wallet, tx20, 1: Long, Seq()).get,
         SimpleBoxTransaction.create(wallet, tx20, 1: Long, Seq()).get,
         SimpleBoxTransaction.create(wallet, tx20, 1: Long, Seq()).get,
         SimpleBoxTransaction.create(wallet, tx30, 1: Long, Seq()).get
      )
      val txs3 = Seq(
         SimpleBoxTransaction.create(wallet, tx10, 1: Long, Seq()).get,
         SimpleBoxTransaction.create(wallet, tx20, 1: Long, Seq()).get
      )

      val txs4 = Seq(
         SimpleBoxTransaction.create(wallet, tx30, 1: Long, Seq()).get
      )

      val genesisBlock = new PowBlock(
         settings.miningSettings.GenesisParentId,
         System.currentTimeMillis(),
         1,
         Digest32 @@ Array.fill(32) (1 : Byte),
         genesisAccount._2,
         BeneficiaryAddress @@ genesisAccount._2,
         Seq()
      )
      val block1 = new PowBlock(
         ModifierId @@ genesisBlock.id,
         System.currentTimeMillis(),
         100 << 30,
         Digest32 @@ Array.fill(32) (1 : Byte),
         genesisAccount._2,
         BeneficiaryAddress @@ genesisAccount._2,
         txs1
      )
      val block2 = new PowBlock(
         ModifierId @@ block1.id,
         System.currentTimeMillis(),
         120 << 40,
         Digest32 @@ Array.fill(32) (1 : Byte),
         genesisAccount._2,
         BeneficiaryAddress @@ genesisAccount._2,
         txs2
      )
      val block3 = new PowBlock(
         ModifierId @@ block2.id,
         System.currentTimeMillis(),
         80 << 30,
         Digest32 @@ Array.fill(32) (1 : Byte),
         genesisAccount._2,
         BeneficiaryAddress @@ genesisAccount._2,
         txs3
      )
      val block4 = new PowBlock(
         ModifierId @@ block3.id,
         System.currentTimeMillis(),
         100 << 40,
         Digest32 @@ Array.fill(32) (1 : Byte),
         genesisAccount._2,
         BeneficiaryAddress @@ genesisAccount._2,
         txs4
      )

      wallet.history = wallet.history.append(genesisBlock).get._1
                                     .append(block1).get._1
                                     .append(block2).get._1
                                     .append(block3).get._1
                                     .append(block4).get._1

      wallet.fullCheckoutInputsOutputs()
      wallet.balance().available shouldBe 230.toLong
      TempDbHelper.del(testFile)
   }

   test("Wallet input calculation with service transactions") {
      val settings = AeneasSettings.read()
      val testFile = TempDbHelper.mkdir

      val storage = new AeneasHistoryStorage (new LSMStore(testFile, maxJournalEntryCount = 100), settings.miningSettings)
      var history = new AeneasHistory(storage, Seq(), settings.miningSettings)

      val genesisAccount = PrivateKey25519Companion.generateKeys("genesisBlock".getBytes, 0)

      val wallet = AeneasWallet.readOrGenerate(history, settings.scorexSettings)
      wallet.generateNewSecret()

      val fundAddress = PublicKey25519Proposition.validPubKey("Æx3fbGHUiSMSC8pHDRCJB1qnNfeykA2XtrvrHrxmaWfdxJdhPPuV").get
      val mastersRewardAddress = PublicKey25519Proposition.validPubKey("Æx2yKq4jutF3Q8CkHgr7gtPJ3rDKpkwNKENVcF6PsbRnRDzebh8r").get
      val ownerSupportAddress = PublicKey25519Proposition.validPubKey("Æx3RCDLUPNp3QnGQqeh88NPTuPBarYiiVivWTVdZULLMrf6Hs73c").get

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

      val genesisBlock = new PowBlock(
         settings.miningSettings.GenesisParentId,
         System.currentTimeMillis(),
         1,
         Digest32 @@ Array.fill(32) (1 : Byte),
         genesisAccount._2,
         BeneficiaryAddress @@ genesisAccount._2,
         Seq()
      )
      val block1 = new PowBlock(
         ModifierId @@ genesisBlock.id,
         System.currentTimeMillis(),
         100 << 30,
         Digest32 @@ Array.fill(32) (1 : Byte),
         wallet.publicKeys.head,
         BeneficiaryAddress @@ wallet.publicKeys.head,
         serviceTx
      )
      val block2 = new PowBlock(
         ModifierId @@ block1.id,
         System.currentTimeMillis(),
         120 << 20,
         Digest32 @@ Array.fill(32) (1 : Byte),
         wallet.publicKeys.head,
         BeneficiaryAddress @@ wallet.publicKeys.head,
         serviceTx
      )

      wallet.history = wallet.history.append(genesisBlock).get._1
                                     .append(block1).get._1
                                     .append(block2).get._1


      wallet.fullCheckoutInputsOutputs()
      wallet.balance().available shouldBe 16000000000L
      TempDbHelper.del(testFile)
   }
}
