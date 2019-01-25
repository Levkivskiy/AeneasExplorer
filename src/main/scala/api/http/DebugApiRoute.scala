package api.http

/**
  * @author luger. Created on 28.03.18.
  * @version ${VERSION}
  */
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import block.{AeneasBlock, PowBlock}
import commons.SimpleBoxTransactionMemPool
import history.AeneasHistory
import io.circe.syntax._
import scorex.core.ModifierId
import scorex.core.api.http.{ApiRouteWithFullView, SuccessApiResponse}
import scorex.core.settings.RESTApiSettings
import scorex.util.encode.Base58
import state.SimpleMininalState
import wallet.AeneasWallet

import scala.concurrent.ExecutionContext.Implicits.global


case class DebugApiRoute(override val settings: RESTApiSettings, nodeViewHolderRef: ActorRef)
  extends ApiRouteWithFullView[AeneasHistory, SimpleMininalState, AeneasWallet, SimpleBoxTransactionMemPool] {

  override val route = pathPrefix("debug") {
    path ("/"){
      complete ("hello")
    }//infoRoute ~ chain ~ delay ~ myblocks ~ generators
  }
  /*
    def delay: Route = path("delay" / Segment / IntNumber) {
          case (encodedSignature, count) =>
            jsonRoute ({
            viewAsync().map { view =>
              SuccessApiResponse(Map(
                "delay" -> Base58.decode(encodedSignature).flatMap(id => view.history.averageDelay(ModifierId @@ id, count))
                  .map(_.toString).getOrElse("Undefined")
              ).asJson)
            }
        }, get)
    }

    def infoRoute: Route = path("info") {
      complete(
        viewAsync().map { view =>
          SuccessApiResponse(Map(
            "height" -> view.history.height.toString.asJson,
            "bestBlock" -> view.history.bestBlock.json,
            "stateVersion" -> Base58.encode(view.state.version).asJson
          ).asJson)
      })
    }

    def myblocks: Route = path("myblocks") {
        viewAsync().map { view =>
          val pubkeys = view.vault.publicKeys

          def isMyPowBlock(b: AeneasBlock): Boolean = b match {
            case pow: PowBlock => pubkeys.exists(_.pubKeyBytes sameElements pow.generatorProposition.pubKeyBytes)
            case _ => false
          }

          val powCount = view.history.count(isMyPowBlock)

          SuccessApiResponse(Map(
            "pubkeys" -> pubkeys.map(pk => Base58.encode(pk.pubKeyBytes)).asJson,
            "count" -> powCount.asJson
          ).asJson)
        }
    }

    def generators: Route = path("generators") {
        viewAsync().map { view =>
          val map: Map[String, Int] = view.history.generatorDistribution()
            .map(d => Base58.encode(d._1.pubKeyBytes) -> d._2)
          SuccessApiResponse(map.asJson)
        }
    }

    def chain: Route = path("chain") {
        viewAsync().map { view =>
          SuccessApiResponse(Map(
            "history" -> view.history.toString
          ).asJson)
      }
    }*/
  override def context: ActorRefFactory = ???
}