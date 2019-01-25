package coreentities

import commons.{SimpleBoxTransactionGenerator, SimpleBoxTransactionSerializer}
import history.storage.AeneasHistoryStorage
import history.{AeneasHistory, TempDbHelper}
import io.iohk.iodb.LSMStore
import org.scalatest.{FunSuite, Matchers}
import settings.AeneasSettings
import wallet.AeneasWallet

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 01.05.18.
  */
class TxSerializationTest extends FunSuite with Matchers {
   test("Non-empty transaction serialization")  {
      val settings = AeneasSettings.read()
      val testFile = TempDbHelper.mkdir
      val store = new LSMStore(testFile, maxJournalEntryCount = 200)
      val storage = new AeneasHistoryStorage(store, settings.miningSettings)
      var history = new AeneasHistory(storage, Seq(), settings.miningSettings)

      val generator = new SimpleBoxTransactionGenerator(AeneasWallet.readOrGenerate(history, settings.scorexSettings))
      val pool = generator.syncGeneratingProcess(10).toSeq

      val serialized : Seq[Array[Byte]] = pool.map(tx => SimpleBoxTransactionSerializer.toBytes(tx))
      val deserialized = serialized.map(bytes => SimpleBoxTransactionSerializer.parseBytes(bytes).get)

      println(pool.head.size() + " " + deserialized.head.size())

      pool.head shouldBe deserialized.head
      pool.zip(deserialized).foreach(el => el._1.size() shouldBe el._2.size())
      pool.zip(deserialized).foreach(el => el._1 shouldBe el._2)
   }
}
