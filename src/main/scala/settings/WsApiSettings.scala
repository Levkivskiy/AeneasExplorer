package settings

import java.io.File
import java.net.InetSocketAddress

/**
  * @author luger. Created on 09.03.18.
  * @version ${VERSION}
  */
case class WsApiSettings (bindAddress: InetSocketAddress, keyPath:String, keyPwd:String){
  val fileDictSrc = new File (keyPath)
//  if (!fileDictSrc.exists()) throw new Error("key not found! Aborting!")
}
