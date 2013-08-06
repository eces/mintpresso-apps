package controllers.v2

import play.api._
import play.api.mvc._
import controllers._

object Nodes extends Controller with Secured {
  
  def findAllByJson(typeName: String) = Signed { implicit request => implicit user =>
    Ok
  }
  
  def findOneByNo(typeName: String, nodeNo: Long) = Signed { implicit request => implicit user =>
    Ok
  }

  def findOneById(typeName: String, nodeId: String) = Signed { implicit request => implicit user =>
    Ok
  }

  def add(typeName: String) = Signed { implicit request => implicit user =>
    Ok
  }

  def addWithJson(typeName: String) = Signed { implicit request => implicit user =>
    Ok
  }

  def delete(typeName: String) = Signed { implicit request => implicit user =>
    Ok
  }
}