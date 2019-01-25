package api.util

import java.security.MessageDigest
import java.util

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import scorex.crypto.encode.Base64

import scala.util.Try

/**
  * @author luger. Created on 12.03.18.
  * @version ${VERSION}
  */
object Encryption {
  def encrypt(key: String, value: String): String = {
    val cipher: Cipher = Cipher.getInstance("AES/ECB/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, keyToSpec(key))
    Base64.encode(cipher.doFinal(value.getBytes("UTF-8")))
  }

  def decrypt(key: String, encryptedValue: String): Try[String] = {
    val cipher: Cipher = Cipher.getInstance("AES/ECB/PKCS5PADDING")
    cipher.init(Cipher.DECRYPT_MODE, keyToSpec(key))
    Base64.decode(encryptedValue).map(ev =>new String(cipher.doFinal(ev)) )
  }

  def keyToSpec(key: String): SecretKeySpec = {
    var keyBytes: Array[Byte] = (SALT + key).getBytes("UTF-8")

    keyBytes = MessageDigest.getInstance("SHA-256").digest(keyBytes)
    keyBytes = util.Arrays.copyOf(keyBytes, 32)
    new SecretKeySpec(keyBytes, "AES")
  }

  private val SALT: String =
    "5UhRU5SKT42fYZJY14AeCMt1mJd516KB7EphvCzedr7v"
}
