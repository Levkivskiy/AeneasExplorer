package history

import java.io.File
import java.util.UUID

import org.apache.commons.io.FileUtils

/**
  * @author luger. Created on 28.02.18.
  * @version ${VERSION}
  */
object TempDbHelper {
  def mkdir: File = {
    val uuid = UUID.randomUUID()
    val testFile = new File(s"${System.getenv("AENEAS_TESTPATH")}/$uuid/blocks")
    del (testFile)
    testFile.mkdirs()
    testFile
  }

  def mkdir2: File = {
    val uuid = UUID.randomUUID()
    val testFile = new File(s"${System.getenv("AENEAS_TESTPATH")}/$uuid/blocks2")
    del (testFile)
    testFile.mkdirs()
    testFile
  }

  def del (f:File) = FileUtils.deleteDirectory(f)
}
