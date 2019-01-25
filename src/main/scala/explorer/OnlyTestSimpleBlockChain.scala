package explorer

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout
import api.SignUpApi
import api.account.{LoginActor, NewAccountEvents}
import com.typesafe.config.ConfigFactory
import commons.SimpleBoxTransaction
import history.AeneasHistory
import io.iohk.iodb.LSMStore
import scorex.core.mainviews.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.core.serialization.SerializerRegistry
import scorex.core.serialization.SerializerRegistry.SerializerRecord
import scorex.core.settings.ScorexSettings
import settings.AeneasSettings
import viewholder.AeneasNodeViewHolder

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration
import akka.pattern.ask

class OnlyTestSimpleBlockChain {

  val loadSettings = LoadSettings()

  private val simpleSettings: AeneasSettings = loadSettings.simpleSettings

  // Note : NEVER NEVER forget to mark implicit as LAZY!
  implicit lazy val settings: ScorexSettings = AeneasSettings.read().scorexSettings

  implicit val serializerReg: SerializerRegistry = SerializerRegistry(Seq(SerializerRecord(SimpleBoxTransaction.simpleBoxEncoder)))

  protected implicit lazy val actorSystem: ActorSystem = ActorSystem("AeneasActors", loadSettings.aeneasActorConfig)

  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new AeneasNodeViewHolder(settings, simpleSettings.miningSettings)))

  private implicit val currentViewTimer: FiniteDuration = 75.second
  private implicit val currentTimeout = new Timeout(currentViewTimer)

  private val storage: LSMStore = {
    val wFile = new File(simpleSettings.scorexSettings.dataDir.getAbsolutePath + File.separator + "account")
    if (!wFile.exists) wFile.mkdirs()
    new LSMStore(wFile, maxJournalEntryCount = 10000)
  }
  val currentViewAwait = ask(nodeViewHolderRef, GetDataFromCurrentView(SignUpApi.applyHistory)).mapTo[AeneasHistory]
  val history: AeneasHistory = Await.result(currentViewAwait, currentViewTimer)

  val loginActor: ActorRef = actorSystem.actorOf(Props(new LoginActor(nodeViewHolderRef, history, simpleSettings.scorexSettings, storage)))

  def savedSeeds() = {
    ask(loginActor, NewAccountEvents.GetSavedSeeds())
  }
  def savedSeedsWithAdress(seed: String) = {
    ask(loginActor, NewAccountEvents.GetSeedWithAddress(seed))
  }

}


case class LoadSettings() {
  val simpleSettings: AeneasSettings = AeneasSettings.read()
  private val root = ConfigFactory.load()
  val aeneasActorConfig = root.getConfig("Aeneas")
  println(aeneasActorConfig.toString)
  // set logging path:
  sys.props += ("log.dir" -> simpleSettings.scorexSettings.logDir.getAbsolutePath)
}
