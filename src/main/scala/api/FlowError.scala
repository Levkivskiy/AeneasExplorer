package api

import akka.stream.scaladsl.Flow
import org.slf4j.Logger

import scala.concurrent.ExecutionContext
import scala.util.Failure

/**
  * @author luger. Created on 19.03.18.
  * @version ${VERSION}
  */
trait FlowError {
  protected def log:Logger
  implicit val executionContext: ExecutionContext

  protected[api] def reportErrorsFlow[T]: Flow[T, T, Any] =
    Flow[T]
      .watchTermination()((_, f) => f.onComplete {
        case Failure(cause) =>
          log.error(s"WS stream failed with $cause")
        case _ => // ignore regular completion
      })
}
