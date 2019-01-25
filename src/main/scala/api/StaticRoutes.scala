package api

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import java.nio.file.{Files, Paths}

import akka.http.scaladsl.model.StatusCodes._
import settings.AeneasSettings

/**
  * @author luger. Created on 21.03.18.
  * @version ${VERSION}
  */
class StaticRoutes(aeneasSettings: AeneasSettings) {
  val workingDirectory = aeneasSettings.staticFilesSettings.staticFilesDir

  private def getExtensions(fileName: String) : String = {

    val index = fileName.lastIndexOf('.')
    if(index != 0) {
      fileName.drop(index+1)
    }else
      ""
  }

  private def getDefaultPage = {

    val fullPath = List(Paths.get(workingDirectory + "/index.html"),Paths.get(workingDirectory + "/index.htm"))
    val res = fullPath.filter(x => Files.exists(x))
    if(res.nonEmpty)
      res.head
    else
      Paths.get("")
  }

  def generate = {
    logRequestResult("aeneas-micro-http-server") {
      get {
        entity(as[HttpRequest]) { requestData =>
          complete {

            val fullPath = requestData.uri.path.toString match {
              case "/"=> getDefaultPage
              case "" => getDefaultPage
              case _ => Paths.get(workingDirectory +  requestData.uri.path.toString)
            }

            val ext = getExtensions(fullPath.getFileName.toString)
            val c : ContentType = ContentType(MediaTypes.forExtensionOption(ext).getOrElse(MediaTypes.`text/plain`), () => HttpCharsets.`UTF-8`)
            println(s"fullPath: $fullPath")
            val byteArray = Files.readAllBytes(fullPath)
            HttpResponse(OK, entity = HttpEntity(c, byteArray))
          }
        }
      }
    }
  }
}
