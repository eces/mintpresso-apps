package controllers.v2

import play.api._
import play.api.mvc._
import controllers._

object Pickups extends Controller with Secured {

  // request.path.endsWith json or xml
  def findOneById(id: String) = TODO

}