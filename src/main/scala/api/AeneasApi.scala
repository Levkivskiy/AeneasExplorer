package api

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.util.Timeout
import api.account.{LoginActor, NewAccActor, SignUpService}
import commons.SimpleBoxTransactionMemPool
import explorer.{Explorer, SocketTrait}
import history.AeneasHistory
import io.iohk.iodb.LSMStore
import scorex.core.mainviews.NodeViewHolder.CurrentView
import scorex.core.mainviews.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.core.utils.ScorexLogging
import settings.AeneasSettings
import state.SimpleMininalState
import wallet.AeneasWallet

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{Await, ExecutionContext}

/**
  * @author luger. Created on 01.03.18.
  * @version ${VERSION}
  */
class AeneasApi(nodeViewHolderRef: ActorRef, aeneasSettingsVal: AeneasSettings, store: LSMStore)(
  implicit systemVal: ActorSystem, executionContextVal: ExecutionContext)
  extends SocketTrait with ScorexLogging {

  private implicit val currentViewTimer: FiniteDuration = 75.second
  private implicit val currentTimeout = new Timeout(currentViewTimer)

  override lazy val aeneasSettings: AeneasSettings = aeneasSettingsVal

  override protected lazy val nodeViewHolder: ActorRef = nodeViewHolderRef

  val currentViewAwait = ask(nodeViewHolderRef, GetDataFromCurrentView(SignUpApi.applyHistory)).mapTo[AeneasHistory]
  val history: AeneasHistory = Await.result(currentViewAwait, currentViewTimer)

  override protected implicit lazy val system: ActorSystem = systemVal

  override implicit val executionContext: ExecutionContext = executionContextVal

  val newAccActor: ActorRef = system.actorOf(Props(new NewAccActor(store)))
  val loginActor: ActorRef = system.actorOf(Props(new LoginActor(nodeViewHolderRef, history, aeneasSettings.scorexSettings, store)))

  override val blockChainExplorer = new Explorer(history, aeneasSettings.scorexSettings, store)

  def route: Route = path("aeneas") {
    handleWebSocketMessages(wsFlow)
  }

}

object SignUpApi {
  def applyHistory(currentView: CurrentView[AeneasHistory,
    SimpleMininalState, AeneasWallet, SimpleBoxTransactionMemPool]): AeneasHistory = {
    currentView.history
  }
}

