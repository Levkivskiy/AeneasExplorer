package state

import java.io.File

import block.{AeneasBlock, PowBlock}
import commons._
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.VersionTag
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{BoxStateChanges, Insertion}
import scorex.core.utils.ScorexLogging
import scorex.util.encode.Base58
import scorex.mid.state.BoxMinimalState

import scala.util.{Success, Try}

/**
  * @author is Alex Syrotenko (@flystyle)
  *         Created on 24.01.18.
  */
case class SimpleMininalState(storage : LSMStore, versionTag: VersionTag) extends BoxMinimalState[PublicKey25519Proposition,
  PublicKey25519NoncedBox,
  SimpleBoxTransaction,
  AeneasBlock,
  SimpleMininalState] with ScorexLogging {
   override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] = storage.get(ByteArrayWrapper(boxId))
     .map(_.data)
     .map(PublicKey25519NoncedBoxSerializer.parseBytes)
     .flatMap(_.toOption)

   override def boxesOf(proposition: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] = ???

   override def changes(mod: AeneasBlock): Try[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
      mod match {
         case pb : PowBlock =>
            val proposition: PublicKey25519Proposition = pb.generatorProposition
            val nonce = SimpleBoxTransaction.nonceFromDigest (mod.id)
            val value = Value @@ 1L
            val minerBox = PublicKey25519NoncedBox (proposition, nonce, value)
            Success (BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox] (Seq (Insertion (minerBox) ) ) )
      }
   }

   override def applyChanges(changes: BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox],
                             newVersion: VersionTag): Try[SimpleMininalState] = Try {
      val boxIdsToRemove = changes.toRemove.map(_.boxId).map(ByteArrayWrapper.apply)
        .ensuring(_.forall(i => closedBox(i.data).isDefined) || storage.lastVersionID.isEmpty)
      val boxesToAdd = changes.toAppend.map(_.box).map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

      log.trace(s"Update HBoxStoredState from version to version ${Base58.encode(newVersion)}. " +
        s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
        s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}")
      storage.update(ByteArrayWrapper(newVersion), boxIdsToRemove, boxesToAdd)
      SimpleMininalState(storage, newVersion)
        .ensuring(st => boxIdsToRemove.forall(box => st.closedBox(box.data).isEmpty), s"Removed box is still in state")
   } ensuring { r => r.toOption.forall(_.version sameElements newVersion )}

   override def validate(mod: AeneasBlock): Try[Unit] = Try {
      mod match {
         case pwb: PowBlock =>
            //coinbase transaction is generated implicitly when block is applied to state, no validation needed
            require(
                  pwb.parentId sameElements version,
                  s"Incorrect state version: ${Base58.encode(version)} " +
                  s"found, ${Base58.encode(pwb.parentId)}"
            )
         case _ =>
      }
   }

   override def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = ???

   // NO ROLLBACK
   override def rollbackTo(version: VersionTag): Try[SimpleMininalState] = ???

   override def version: VersionTag = versionTag

   override def maxRollbackDepth: Int = storage.keepVersions

   override type NVCT = this.type
}

object SimpleMininalState {
   def readOrGenerate(settings: ScorexSettings): SimpleMininalState = {
      import settings.dataDir
      dataDir.mkdirs()

      val iFile = new File(s"${dataDir.getAbsolutePath}/state")
      iFile.mkdirs()
      val storage = new LSMStore(iFile, maxJournalEntryCount = 10000)

      Runtime.getRuntime.addShutdownHook(new Thread() {
         override def run(): Unit = {
            storage.close()
         }
      })
      val version = VersionTag @@ storage.lastVersionID.map(_.data).getOrElse(Array.emptyByteArray)

      SimpleMininalState(storage, version)
   }
   def genesisState(settings: ScorexSettings, initialBlocks: Seq[AeneasBlock]): SimpleMininalState = {
      initialBlocks.foldLeft(readOrGenerate(settings)) {
         (state, mod) =>
            state.changes(mod).flatMap(cs => state.applyChanges(cs, VersionTag @@ mod.id)).get
      }
   }

   def changes(mod: AeneasBlock): Try[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
      mod match {
         case pb: PowBlock =>
            val proposition: PublicKey25519Proposition = pb.generatorProposition
            val nonce: Nonce = SimpleBoxTransaction.nonceFromDigest(mod.id)
            val value: Value = Value @@ 1L
            val minerBox = PublicKey25519NoncedBox(proposition, nonce, value)
            Success(BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Seq(Insertion(minerBox))))
      }
   }
}
