package api.account

import akka.NotUsed
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.scaladsl.{BroadcastHub, Keep, RunnableGraph, Sink, Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy, QueueOfferResult}
import block.PowBlock
import mining.Miner
import scorex.core.utils.{ActorHelper, ScorexLogging}
import settings.AeneasSettings

import scala.concurrent.{ExecutionContext, Future}

/**
  * @author luger. Created on 13.03.18.
  * @version ${VERSION}
  */
trait PowBlocksBroadcast extends ActorHelper {
  protected def nodeViewHolder:ActorRef
  protected def aeneasSettings: AeneasSettings
  protected implicit def system: ActorSystem
  protected implicit def executionContext: ExecutionContext

  implicit protected val materializer: ActorMaterializer = ActorMaterializer()

  val bufferSize = 256

  //if the buffer fills up then this strategy drops the oldest elements
  //upon the arrival of a new element.

  private val overflowStrategy: OverflowStrategy = akka.stream.OverflowStrategy.dropHead

  private val source: Source[PowBlock, SourceQueueWithComplete[PowBlock]] = Source.queue(
    bufferSize, overflowStrategy
  )

  private val queue: RunnableGraph[(SourceQueueWithComplete[PowBlock], Source[PowBlock, NotUsed])] =
    source.toMat( BroadcastHub.sink(bufferSize) )(Keep.both)

  val producer: (SourceQueueWithComplete[PowBlock], Source[PowBlock, NotUsed]) = queue.run()

  producer._2.runWith(Sink.ignore)


  def publishBlock (pb:PowBlock): Future[QueueOfferResult] = {
    producer._1.offer(pb)
  }

  system.actorOf(Props(new InformatorActor(nodeViewHolder, this)))
}

class InformatorActor(val nodeViewHolderRef:ActorRef, powBlocksBroadcast:PowBlocksBroadcast) extends Actor with ScorexLogging {
  override def preStart(): Unit = {
    super.preStart()
    nodeViewHolderRef ! Miner.UiInformatorSubscribe
  }

  override def receive: Receive = {
    case pb:PowBlock =>
      log.debug(s"pb txs:${pb.transactions.size}")
      powBlocksBroadcast.publishBlock(pb)
    case x =>
      log.debug(s"undefined info : ${x}")
  }
}