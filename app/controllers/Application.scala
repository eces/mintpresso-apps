package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Json._

object Application extends Controller with Secured {
  
  def index = Action {
    Ok(views.html.index())
  }

  def authorize = Signed("*") { implicit request => user =>
    Ok("authorize.ok")
  }

  def findTypesByOwnerNo(ownerNo: Long) = Signed("read_type") { implicit request => user =>
    import models.{Edge, User}

    val json = models.Node.findTypesByOwnerNo(User.Empty(ownerNo)).foldLeft(Json.arr()){ (a, b) =>
      a.append(Json.obj("no" -> b._1, "name" -> b._2))
    }
    Ok(json)
  }

  def findVerbsByOwnerNo(ownerNo: Long) = Signed("read_verb") { implicit request => user =>
    import models.{Edge, User}

    val json = Edge.findVerbsByOwnerNo(User.Empty(ownerNo)).foldLeft(Json.arr()){ (a, b) =>
      a.append(Json.obj("verb" -> b))
    }
    Ok(json)
  }
  
}