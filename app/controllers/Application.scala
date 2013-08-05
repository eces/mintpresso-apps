package controllers

import play.api._
import play.api.mvc._

object Application extends Controller with Secured {
  
  def index = Action {
    Ok(views.html.index())
  }

  def authorize = Signed { implicit request => user =>
    Ok("authorize.ok")
  }
  
}