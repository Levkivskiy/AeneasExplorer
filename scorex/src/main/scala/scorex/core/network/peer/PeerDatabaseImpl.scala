package scorex.core.network.peer

import java.net.InetSocketAddress

import scorex.core.utils.{NetworkTime, ScorexLogging}

import scala.collection.mutable


//todo: persistence
class PeerDatabaseImpl(filename: Option[String]) extends PeerDatabase with ScorexLogging {

  private val whitelistPersistence = mutable.Map[InetSocketAddress, PeerInfo]()

  private val blacklist = mutable.Map[String, NetworkTime.Time]()

  private def printPeersToLog() : Unit = {
    log.debug(s" === Peer database printing begins === ")
    whitelistPersistence.foreach {
       case (key, value) => log.debug(s" Peer : ${key.toString} -> [${value.toString}]")
       case _ =>
    }
    log.debug(s" === Peer database printing ends === ")
  }

  override def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo = whitelistPersistence.get(address).fold(peerInfo) { dbPeerInfo =>
      val nodeNameOpt = peerInfo.nodeName orElse dbPeerInfo.nodeName
      val connTypeOpt = peerInfo.connectionType orElse  dbPeerInfo.connectionType
      PeerInfo(peerInfo.lastSeen, nodeNameOpt, connTypeOpt)
    }
    whitelistPersistence.put(address, updatedPeerInfo)
  }

  override def blacklistPeer(address: InetSocketAddress, time: NetworkTime.Time): Unit = {
    whitelistPersistence.remove(address)
    if (!isBlacklisted(address)) blacklist += address.getHostName -> time
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getHostName))
  }

  override def knownPeers(): Map[InetSocketAddress, PeerInfo] = {
    whitelistPersistence.keys.flatMap(k => whitelistPersistence.get(k).map(v => k -> v)).toMap
  }

  override def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

  override def isEmpty(): Boolean = whitelistPersistence.isEmpty
}
