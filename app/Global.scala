import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.concurrent._
import play.api.Play.current

object Global extends GlobalSettings {

  override def onStart(app: Application) {

  }  
  
  override def onStop(app: Application) {

  }

  override def onError(request: RequestHeader, ex: Throwable) = {
    Logger.warn(s"[Error] ${request}")
    Logger.warn(s"[Error] ${ex}")
    Results.InternalServerError
  }

  override def onHandlerNotFound(request: RequestHeader): Result = {
    Logger.warn(s"[HandlerNotFound] ${request}")
    Results.NotFound
  }

  override def onBadRequest(request: RequestHeader, error: String) = {
    Logger.warn(s"[BadRequest] ${request}")
    Logger.warn(s"[BadRequest] ${error}")
    Results.BadRequest
  }
    
}