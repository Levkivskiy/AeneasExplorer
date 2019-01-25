package commons

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.{Encoder, Json}
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.ModifierId
import scorex.core.serialization.{JsonSerializable, Serializer}
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.box.proposition.PublicKey25519Proposition.validPubKey
import scorex.core.transaction.box.proposition.PublicKey25519PropositionSerializer.parseBytes
import scorex.core.transaction.proof.{Proof, Signature25519}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.util.encode.Base58
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}
import wallet.AeneasWallet

import scala.util.{Failure, Try}

// A transaction orders to destroy boxes associated with (pubkey -> nonce) and create new boxes (pubkey -> nonce)
// where a nonce is derived from a transaction and also a box index

// WARNING!: the scheme is not provably secure to replay attacks etc.

// This file was transfered from Twinscoin example.

case class SimpleBoxTransaction(from: IndexedSeq[(PublicKey25519Proposition, Value)],
                                to: IndexedSeq[(PublicKey25519Proposition, Value)],
                                signatures: IndexedSeq[Signature25519],
                                override val fee: Long,
                                override val timestamp: Long) extends
  BoxTransaction[PublicKey25519Proposition, PublicKey25519NoncedBox] with JsonSerializable {

  override type M = SimpleBoxTransaction

  lazy val boxIdsToOpen: IndexedSeq[ModifierId] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }.distinct

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.zip(signatures).map {
    case (boxId, signature) =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: ModifierId = boxId
        override val boxKey: Proof[PublicKey25519Proposition] = signature
      }
  }

  lazy val hashNoNonces: Digest32 = Blake2b256(
    Bytes.concat(scorex.core.utils.concatFixLengthBytes(to.map(_._1.pubKeyBytes)),
      scorex.core.utils.concatFixLengthBytes(unlockers.map(_.closedBoxId)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
  )

  override lazy val newBoxes: Traversable[PublicKey25519NoncedBox] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = SimpleBoxTransaction.nonceFromDigest(Blake2b256(prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
      PublicKey25519NoncedBox(prop, nonce, value)
  }

  override lazy val serializer = SimpleBoxTransactionSerializer

  lazy val idToString: String = Base58.encode(id)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).toSeq.asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> from.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "nonce" -> s._2.toLong.asJson
      ).asJson
    }.asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "value" -> s._2.toLong.asJson
      ).asJson
    }.asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  import PublicKey25519Proposition.validPubKey


  override def toString: String = s"SimpleBoxTransaction(${json.noSpaces})"

  lazy val uncodeJson: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).toSeq.asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> from.map { s =>
      Map(
        "proposition" -> s._1.address.asJson,
        "nonce" -> s._2.toLong.asJson
      ).asJson
    }.asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> s._1.address.asJson,
        "value" -> s._2.toLong.asJson
      ).asJson
    }.asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  lazy val semanticValidity: Try[Unit] = Try {
    require(from.size == signatures.size)
    require(to.forall(_._2 >= 0))
    require(fee >= 0)
    require(timestamp >= 0)
    require(boxIdsToOpen.map(to => ByteArrayWrapper(to)).distinct.size == boxIdsToOpen.size)
    require(from.zip(signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, messageToSign)
    })
  }

  def size() : Int = {
    signatures.length * Curve25519.SignatureLength  // metainformation length && signatures length
    + (Curve25519.KeyLength + 8) * from.length          // inputs length
    + (Curve25519.KeyLength + 8) * to.length + 28
  }
}

object SimpleBoxTransaction {

  def uncode(oldTrand: SimpleBoxTransaction) = {


    SimpleBoxTransaction(
    oldTrand.from,//.map(a => (parseBytes(Base58.decode(a._1.address).get).get, a._2)),
    oldTrand.to,//.map(a => (parseBytes(Base58.decode(a._1.address).get).get, a._2)),
    oldTrand.signatures,
    oldTrand.fee,
    oldTrand.timestamp
  )}

  val theMostLessTxSize: Int = 4 + 28
//oldTrand.to.map(a => (parseBytes(Base58.decode(a._1.address).get).get, a._2)),
  def idFromString(id: String): Try[Array[Byte]] = Base58.decode(id)

  implicit val simpleBoxEncoder: Encoder[SimpleBoxTransaction] =
    (sbe: SimpleBoxTransaction) => sbe.json

  def nonceFromDigest(digest: Array[Byte]): Nonce = Nonce @@ Longs.fromByteArray(digest.take(8))

  def apply(from: IndexedSeq[(PrivateKey25519, Value)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long): SimpleBoxTransaction = {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
    val fakeSigs = from.map(_ => Signature25519(Signature @@ Array[Byte]()))

    val undersigned = SimpleBoxTransaction(fromPub, to, fakeSigs, fee, timestamp)

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }

    new SimpleBoxTransaction(fromPub, to, sigs, fee, timestamp)
  }

  def create(w: AeneasWallet,
             to: (PublicKey25519Proposition, Value),
             fee: Long,
             boxesIdsToExclude: Seq[Array[Byte]] = Seq()): Try[SimpleBoxTransaction] = Try {
    val amount = to._2
    if (w.balance().available < amount)
      Failure(new Exception("Not enough funds!"))

    val fakeSignature = Signature25519(Signature @@ Array[Byte]())
    val nonsigned = new SimpleBoxTransaction(
      IndexedSeq(),
      IndexedSeq(
        to._1 -> Value @@ amount,
        w.publicKeys.head -> Value @@ -amount
      ),
      IndexedSeq(fakeSignature),
      fee,
      System.currentTimeMillis()
    )
    val msg = nonsigned.messageToSign
    val privateKey = w.secretByPublicImage(w.publicKeys.head) match {
      case Some(key) => key
      case None => w.secrets.firstKey
    }
    val realSignature = PrivateKey25519Companion.sign(privateKey, msg)

    new SimpleBoxTransaction(
      IndexedSeq(w.publicKeys.head -> Value @@ -amount),
      IndexedSeq(
        to._1 -> Value @@ amount,
        w.publicKeys.head -> Value @@ -amount
      ),
      IndexedSeq(realSignature),
      fee,
      System.currentTimeMillis()
    )
  }

  def createFee(from: PublicKey25519Proposition,
             to: (PublicKey25519Proposition, Value)): SimpleBoxTransaction = {
    val amount = to._2
    val feeSignature = Signature25519(Signature @@ Array[Byte](1, 3, 3, 7, 13, 37))

    new SimpleBoxTransaction(
      IndexedSeq(from -> Value @@ -amount),
      IndexedSeq(
        to._1 -> Value @@ amount,
        from -> Value @@ -amount
      ),
      IndexedSeq(feeSignature),
      0,
      System.currentTimeMillis()
    )
  }

}


object SimpleBoxTransactionSerializer extends Serializer[SimpleBoxTransaction] {

  override def toBytes(m: SimpleBoxTransaction): Array[Byte] = {
    Bytes.concat(Longs.toByteArray(m.fee),
         Longs.toByteArray(m.timestamp),
         Ints.toByteArray(m.signatures.length),
         Ints.toByteArray(m.from.length),
         Ints.toByteArray(m.to.length),
         m.signatures.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b.bytes)),
         m.from.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b._1.bytes, Longs.toByteArray(b._2))),
         m.to.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b._1.bytes, Longs.toByteArray(b._2)))
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[SimpleBoxTransaction] = Try {
    val fee = Longs.fromByteArray(bytes.slice(0, 8))
    val timestamp = Longs.fromByteArray(bytes.slice(8, 16))
    val sigLength = Ints.fromByteArray(bytes.slice(16, 20))
    val fromLength = Ints.fromByteArray(bytes.slice(20, 24))
    val toLength = Ints.fromByteArray(bytes.slice(24, 28))
    val signatures = (0 until sigLength) map { i =>
      Signature25519(Signature @@ bytes.slice(28 + i * Curve25519.SignatureLength, 28 + (i + 1) * Curve25519.SignatureLength))
    }

    val s = 28 + sigLength * Curve25519.SignatureLength
    val elementLength = 8 + Curve25519.KeyLength
    val from = (0 until fromLength) map { i =>
      val pk = PublicKey @@ bytes.slice(s + i * elementLength, s + (i + 1) * elementLength - 8)
      val v = Longs.fromByteArray(bytes.slice(s + (i + 1) * elementLength - 8, s + (i + 1) * elementLength))
      (PublicKey25519Proposition(pk), Value @@ v)
    }

    val s2 = s + fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val pk = PublicKey @@ bytes.slice(s2 + i * elementLength, s2 + (i + 1) * elementLength - 8)
      val v = Longs.fromByteArray(bytes.slice(s2 + (i + 1) * elementLength - 8, s2 + (i + 1) * elementLength))
      (PublicKey25519Proposition(pk), Value @@ v)
    }
    SimpleBoxTransaction(from, to, signatures, fee, timestamp)
  }
}