package settings

import java.io.File


/**
  * @author luger. Created on 09.03.18.
  * @version ${VERSION}
  */
case class SeedSettings(passPhraseSize: Int = 15, file:String){
  println(System.getenv("WORDSDICT"))
  val fileDictSrc = Option(new File(file)).filter(_.exists()).getOrElse(new File(Option (getClass.getResource("/"+file)).map(_.getPath).getOrElse("")))
  if (!fileDictSrc.exists()) throw new Error("Malformed words dictionary file was provided! Aborting!")

}
