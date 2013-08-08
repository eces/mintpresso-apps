import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.concurrent._
import play.api.Play.current

object Global extends GlobalSettings {
  
  val APIFilter = Filter { (next, rh) =>
    val d = new java.util.Date
    rh.path match {
      case p if p == "/" => next(rh)
      case p if p.endsWith(".json") => {
        next(rh).withHeaders (
          "Content-Type" -> "application/json; charset=utf-8",
          "Cache-Control" -> "max-age=86400",
          "X-Content-Type-Options" -> "nosniff"
        )
      }
      case p if p.endsWith(".xml") => {
        next(rh).withHeaders (
          "Content-Type" -> "application/xml; charset=utf-8",
          "Cache-Control" -> "max-age=86400",
          "X-Content-Type-Options" -> "nosniff"
        )
      }
      case _ => {
        next(rh).withHeaders (
          "Date" -> d.getTime.toString,
          "Content-Type" -> "application/json; charset=utf-8",
          "Cache-Control" -> "max-age=0, private, must-revalidate",
          "X-Content-Type-Options" -> "nosniff"
        )
      }
    }
  }

  override def doFilter(action: EssentialAction) = {
    APIFilter(action)
  }

  override def onStart(app: Application) {
    import models._
    Node.findOneByNo(1) match {
      case Some(n) => {}
      case None => {
        val user = User.Default
        user.password = "reset"
        Node( user.toTypedJson ).saveWithNo
        Logger.info(s"user(app@mintpresso.com) created.")
      }
    }
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