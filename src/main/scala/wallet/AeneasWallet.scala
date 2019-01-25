package wallet

import java.io.File
import java.util.NoSuchElementException
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import block.{AeneasBlock, PowBlock}
import com.google.common.primitives.Ints
import commons._
import history.AeneasHistory
import history.storage.AeneasHistoryStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.{ModifierId, VersionTag}
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion, PrivateKey25519Serializer}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletBoxSerializer, WalletTransaction}
import scorex.core.utils.{ByteStr, ScorexLogging}
import scorex.util.encode.Base58
import scorex.crypto.hash.Blake2b256
import settings.AeneasSettings
import state.SimpleMininalState

import scala.collection.immutable.TreeSet
import scala.util.Try
import AddOpts._
import AddInstances._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition.validPubKey

import scala.annotation.tailrec

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 09.02.18.
  */

case class AeneasWallet(var history: AeneasHistory, seed: ByteStr, store: LSMStore, balanceInit: Balance = Balance(0l, 0l, 0l))
  extends Wallet[PublicKey25519Proposition, SimpleBoxTransaction, AeneasBlock, AeneasWallet]
    with ScorexLogging {

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  private val SecretsKey: ByteArrayWrapper = if (seed.base58.nonEmpty) ByteArrayWrapper(seed.arr.take(store.keySize) ++ Array.fill(Math.max(0, store.keySize - seed.arr.length))(2: Byte)) else ByteArrayWrapper(Array.fill(store.keySize)(2: Byte))

  private val BoxIdsKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(store.keySize)(1: Byte))

  private val availableBalance: AtomicReference[Balance] = new AtomicReference[Balance](balanceInit)

  def balance() = availableBalance.get()

  def boxIds: Seq[Array[Byte]] = {
    store.get(BoxIdsKey).map(_.data.grouped(store.keySize).toSeq).getOrElse(Seq[Array[Byte]]())
  }

  private lazy val walletBoxSerializer =
    new WalletBoxSerializer[PublicKey25519Proposition, PublicKey25519NoncedBox](PublicKey25519NoncedBoxSerializer)

  //TODO: implement it.
  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, SimpleBoxTransaction]] = ???

  def decodeKey(powBlock: PowBlock): PowBlock = {

    log.info(powBlock.transactionPool.map(_.to.map(_._1)).toString)
    new PowBlock(
      powBlock.parentId,
      powBlock.timestamp,
      powBlock.nonce,
      powBlock.merkleRoot,
      powBlock.generatorProposition,
      powBlock.beneficiaryAddress,
      powBlock.transactionPool.map(a => SimpleBoxTransaction.uncode(a)),
      powBlock.number
    )
  } //adr.to.map(a => validPubKey(Base58.decode(a._1.address).get).get)

  override def boxes(): Seq[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    boxIds
      .flatMap(id => store.get(ByteArrayWrapper(id)))
      .map(_.data)
      .map(ba => walletBoxSerializer.parseBytes(ba))
      .map(_.get)
      .filter(_.box.value > 0)
  }

  // "to":[{"proposition":"2wPETqKVgQCntuTGbHByBXTKWuei67rp2XeMXZD7XQQZ","value":10000000000},{"proposition":"4BrtZ5xPoX1ceGWKWjqhrNcRVmB94o93ZLUgUPQzG5ZX","value":-10000099986}],
  // "id":"5QCYTfRDDKumu5wJiFsyPN8SA7GS9gyaFj2iyqFjtpT4",
  // "fee":100000}"

  /**
    * It counts if sum of inputs in this block.
    *
    * @param acquiredBlock block.
    */
  private[wallet] def extractInputs(acquiredBlock: PowBlock, pubKeySet: Set[PublicKey25519Proposition]): Long = {
    acquiredBlock.transactionPool
      .flatMap(tx => tx.to.filter(in => pubKeySet.contains(in._1) && in._2 > 0))
      .foldLeft(0: Long) { (acc, in) => acc + in._2 }
  }

  /**
    * It counts if sum of outputs in this block (with negative sign).
    *
    * @param acquiredBlock block.
    */
  private[wallet] def extractOuts(acquiredBlock: PowBlock, pubKeySet: Set[PublicKey25519Proposition]): Long = {
    acquiredBlock.transactionPool
      .flatMap(tx => tx.to.filter(in => pubKeySet.contains(in._1) && in._2 < 0))
      .foldLeft(0: Long) { (acc, in) => acc + in._2 }
  }


  //
  /**
    * It makes full traverse of chain to determine transactions and inputs/outputs which are related to current wallet.
    * It is very expensive operation!
    * Simple Example :
    *
    * Block 1: input (A, 20)
    * Block 2: input (B, 30)
    * Block 3: input (B, 30)
    * Block 4: out (C, 20)
    * Block 5: out (C, 20)
    * Result : available inputs : (B, 10), (B, 30) => 40.
    *
    * @return available inputs.
    */
  def fullCheckoutInputsOutputs(pubKeySet: Set[PublicKey25519Proposition]): Either[ModifierId, Balance] = {
    @tailrec
    def loop(id: ModifierId, pid: ModifierId, accBalance: Balance): (ModifierId, Balance) = history.modifierById(id) match {
      case Some(acquiredBlock: PowBlock) =>
        val extractedInputs = extractInputs(acquiredBlock, pubKeySet)
        val extractedOuts = extractOuts(acquiredBlock, pubKeySet)
        if (acquiredBlock.number % 1000 == 0) log.info(s"loop curent block is ${acquiredBlock.number} Trying find block")
        if (extractedInputs != 0 || extractedOuts != 0) log.info(s"Block ${acquiredBlock.number} has ${acquiredBlock.transactionPool.size} transaction(s)")
        if (extractedInputs != 0) log.info(s"$extractedInputs AE was extracted as IN from block #${acquiredBlock.number}")
        if (extractedOuts != 0) log.info(s"$extractedOuts AE was extracted as OUT from block #${acquiredBlock.number}")

        val b = Balance(0, extractedInputs, 0) :+ Balance(0, extractedOuts, 0)
        loop(acquiredBlock.parentId, id, accBalance :+ b)
      case _ => (pid, accBalance)
    }

    val (id, b) = loop(history.bestBlock().id, history.bestBlock().id, Balance(0l, 0l, 0l))
    availableBalance.set(b)
    if (id.deep == history.genesis().deep) Right(b)
    else Left(id)
  }

  def countMinedBlocks(pubKey: PublicKey25519Proposition): Int = {
    @tailrec
    def loop(id: ModifierId, pid: ModifierId, mineblocks: Int): (ModifierId, Int) = history.modifierById(id) match {
      case Some(acquiredBlock: PowBlock) =>
        val mined = if (acquiredBlock.generatorProposition == pubKey) 1 else 0
        if (mined == 1) log.info(pubKey.address + s" Mined block ${acquiredBlock.number}")
        loop(acquiredBlock.parentId, id, mined.asInstanceOf[Int] + mineblocks)
      case _ => (pid, mineblocks)
      //re
    }

    val (_, resMined) = loop(history.bestBlock().id, history.bestBlock().id, 0)
    resMined
  }

  def fullCheckoutInputsOutputs(): Either[ModifierId, Balance] = {
    fullCheckoutInputsOutputs(publicKeys)
  }

  private def isMinerBenefits(transaction: SimpleBoxTransaction) =
    transaction.to.head._2 == 8000000000L && transaction.to.size == 1

  private def extractInputTrans(acquiredBlock: PowBlock, publicKey: PublicKey25519Proposition): Seq[SimpleBoxTransaction] =
    acquiredBlock.transactionPool
      .filter(tx => tx.to.exists(x => x._1.equals(publicKey) && x._2 > 0) && !isMinerBenefits(tx))

  private def extractOutputTrans(acquiredBlock: PowBlock, publicKey: PublicKey25519Proposition): Seq[SimpleBoxTransaction] =
    acquiredBlock.transactionPool
      .filter(tx => tx.to.exists(x => x._1.equals(publicKey) && x._2 < 0))


  def historyTransactions(publicKey: PublicKey25519Proposition): Seq[SimpleBoxTransaction] = {
    @tailrec
    def loop(id: ModifierId, pid: ModifierId, transSet: Seq[SimpleBoxTransaction]): (ModifierId, Seq[SimpleBoxTransaction]) = {
      history.modifierById(id) match {
        case Some(acquiredBlock: PowBlock) =>
          val calcInputTrans = extractInputTrans(acquiredBlock, publicKey)
          val calcOutputTrans = extractOutputTrans(acquiredBlock, publicKey)

          if (calcInputTrans.nonEmpty) calcInputTrans.foreach(trans => log.info(s"INPUT transaction ${
            trans
          } was extracted as IN from block #${acquiredBlock.number}"))
          if (calcOutputTrans.nonEmpty) calcOutputTrans.foreach(trans => log.info(s"OUTPUT transaction ${
            trans
          } was extracted as IN from block #${acquiredBlock.number}"))

          loop(acquiredBlock.parentId, id, transSet ++ calcInputTrans ++ calcOutputTrans)
        case _ => (pid, transSet)
      }
    }

    val (_, hist) = loop(history.bestBlock().id, history.bestBlock().id, Seq[SimpleBoxTransaction]())
    hist
  }

  def findBlock(compare: PowBlock => Boolean) = {

    @tailrec
    def loop(id: ModifierId, pid: ModifierId): Option[PowBlock] = {
      history.modifierById(id) match {
        case Some(acquiredBlock: PowBlock) if compare(acquiredBlock) =>
          Some(acquiredBlock)
        case Some(acquiredBlock: PowBlock) =>
          if (acquiredBlock.number % 1000 == 0) log.info(s"loop curent block is ${acquiredBlock.number} Trying find block")
          loop(acquiredBlock.parentId, id)
        case _ => None
      }
    }

    loop(history.bestBlock().id, history.bestBlock().id)
  }

  def newBlockToBalance(acquiredBlock: PowBlock): Balance = {
    val extractedInputs = extractInputs(acquiredBlock, publicKeys)
    log.info(s"$extractedInputs AE was extracted as IN from block #${acquiredBlock.number}")
    log.debug(s"Balance : ${balance()}, add: ${Balance(0, extractedInputs, 0)}")
    availableBalance.getAndSet(availableBalance.get() :+ Balance(0, extractedInputs, 0))

    val extractedOuts = extractOuts(acquiredBlock, publicKeys)
    log.info(s"$extractedOuts AE was extracted as OUT from block #${acquiredBlock.number}")
    availableBalance.getAndSet(availableBalance.get() :+ Balance(0, extractedOuts, 0))
    availableBalance.get()
  }


  override def publicKeys: Set[PublicKey25519Proposition] = secrets.map(_.publicImage)

  override def secrets: TreeSet[PrivateKey25519] = store.get(SecretsKey)
    .map(_.data.grouped(68).map(b => PrivateKey25519Serializer.parseBytes(b).get).to[TreeSet])
    .getOrElse(TreeSet.empty[PrivateKey25519])

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    secrets.find(s => s.publicImage == publicImage)

  implicit def orderingByOrder[A <: PrivateKey25519]: Ordering[A] =
    Ordering.by(e => e.order)

  override def generateNewSecret(): AeneasWallet = {
    val prevSecrets = secrets
    val nonce: Array[Byte] = Ints.toByteArray(prevSecrets.size)
    val s = Blake2b256(seed.arr ++ nonce)
    val (priv, _) = PrivateKey25519Companion.generateKeys(s, secrets.size)
    val allSecrets: TreeSet[PrivateKey25519] = TreeSet(priv) ++ prevSecrets
    log.debug(s"allSecrets:$allSecrets")

    store.update(ByteArrayWrapper(priv.privKeyBytes),
      Seq(),
      Seq(SecretsKey -> ByteArrayWrapper(allSecrets.toArray.flatMap(p => PrivateKey25519Serializer.toBytes(p)))))
    AeneasWallet(history, seed, store)
  }

  override def scanOffchain(tx: SimpleBoxTransaction): AeneasWallet = this

  override def scanOffchain(txs: Seq[SimpleBoxTransaction]): AeneasWallet = this

  override def scanPersistent(modifier: AeneasBlock): AeneasWallet = {
    log.debug(s"Applying modifier to wallet: ${Base58.encode(modifier.id)}")
    val changes = SimpleMininalState.changes(modifier).get

    val newBoxes = changes.toAppend.filter(s => secretByPublicImage(s.box.proposition).isDefined).map(_.box).map { box =>
      val boxTransaction = modifier.transactions.find(t => t.newBoxes.exists(tb => tb.id sameElements box.id))
      val txId = boxTransaction.map(_.id).getOrElse(Array.fill(32)(0: Byte))
      val ts = boxTransaction.map(_.timestamp).getOrElse(modifier.timestamp)
      val wb = WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox](box, txId, ts)(PublicKey25519NoncedBoxSerializer)
      ByteArrayWrapper(box.id) -> ByteArrayWrapper(wb.bytes)
    }

    val boxIdsToRemove = changes.toRemove.view.map(_.boxId).map(ByteArrayWrapper.apply)
    val newBoxIds: ByteArrayWrapper = ByteArrayWrapper(newBoxes.toArray.flatMap(_._1.data) ++
      boxIds.filter(bi => !boxIdsToRemove.exists(_.data sameElements bi)).flatten)
    store.update(ByteArrayWrapper(modifier.id), boxIdsToRemove, Seq(BoxIdsKey -> newBoxIds) ++ newBoxes)
    log.debug(s"Successfully applied modifier to wallet: ${Base58.encode(modifier.id)}")

    AeneasWallet(history, seed, store, balance())
  }

  override def rollback(to: VersionTag): Try[AeneasWallet] = Try {
    if (store.lastVersionID.exists(_.data sameElements to)) {
      this
    } else {
      log.debug(s"Rolling back wallet to: ${Base58.encode(to)}")
      store.rollback(ByteArrayWrapper(to))
      log.debug(s"Successfully rolled back wallet to: ${Base58.encode(to)}")
      AeneasWallet(history, seed, store)
    }
  }

  override type NVCT = this.type

}

case class Balance(total: Long, available: Long, unconfirmed: Long)

trait Add[A] {
  def add(a: A, b: A): A
}

object Add {
  def add[A](a: A, b: A)(implicit add: Add[A]) = {
    add.add(a, b)
  }
}

object AddOpts {

  implicit class Opts[A](a: A) {
    def :+(b: A)(implicit add: Add[A]): A = add.add(a, b)
  }

}

object AddInstances {
  implicit val BalanceAdd: Add[Balance] = (a: Balance, b: Balance) => Balance(a.total + b.total, a.available + b.available, a.unconfirmed + b.unconfirmed)
}

object AeneasWallet extends ScorexLogging {

  def walletFile(settings: ScorexSettings): File = {
    if (!settings.wallet.walletDir.exists())
      settings.wallet.walletDir.mkdirs()
    new File(s"${settings.wallet.walletDir.getAbsolutePath}") //TODO WTF?????
  }

  def exists(settings: ScorexSettings): Boolean = walletFile(settings).exists()

  def nonEmpty(settings: ScorexSettings): Boolean = walletFile(settings).listFiles().exists(_.isFile)

  def readOrGenerate(history: AeneasHistory, settings: ScorexSettings, seed: ByteStr): AeneasWallet = {
    val wFile = settings.wallet.walletDir
    if (wFile.exists) settings.wallet.walletDir.mkdirs
    else {
      settings.wallet.walletDir.getParentFile.mkdirs
      settings.wallet.walletDir.mkdirs
      settings.wallet.walletDir.createNewFile
    }
    val boxesStorage = new LSMStore(wFile, maxJournalEntryCount = 10000)
    sys.addShutdownHook {
      boxesStorage.close()
    }

    AeneasWallet(history, seed, boxesStorage)
  }

  def readOrGenerate(history: AeneasHistory, settings: ScorexSettings): AeneasWallet = {
    readOrGenerate(history, settings, settings.wallet.seed)
  }

  def emptyHistoryReadOrGenerate(settings: AeneasSettings): AeneasWallet = {
    val wFile = settings.scorexSettings.wallet.walletDir
    readOrGenerate(new AeneasHistory(new AeneasHistoryStorage(new LSMStore(wFile, maxJournalEntryCount = 10000),
      settings.miningSettings),
      Seq(),
      settings.miningSettings),
      settings.scorexSettings, settings.scorexSettings.wallet.seed)
  }

  def readOrGenerate(history: AeneasHistory, settings: ScorexSettings, seed: ByteStr, accounts: Int): AeneasWallet =
    (1 to accounts).foldLeft(readOrGenerate(history, settings, seed)) { case (w, _) =>
      w.generateNewSecret()
    }

  def readOrGenerate(history: AeneasHistory, settings: ScorexSettings, accounts: Int): AeneasWallet =
    (1 to accounts).foldLeft(readOrGenerate(history, settings)) { case (w, _) =>
      w.generateNewSecret()
    }

  //wallet with applied initialBlocks
  def genesisWallet(history: AeneasHistory, settings: ScorexSettings, initialBlocks: Seq[AeneasBlock]): AeneasWallet = {
    initialBlocks.foldLeft(readOrGenerate(history, settings).generateNewSecret()) { (a, b) =>
      a.scanPersistent(b)
    }
  }
}

case class AeneasInput(proposition: PublicKey25519Proposition, value: Value) extends Ordered[AeneasInput] {
  override def compare(that: AeneasInput): Int = this.value compare that.value
}
