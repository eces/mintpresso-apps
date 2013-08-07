package controllers.v2

import play.api._
import play.api.mvc._
import controllers._

object Nodes extends Controller with Secured {
  
  def findAllByJson(typeName: String) = Signed("search_model") { implicit request => implicit user =>
    Ok
  }
  
  def findOneByNo(typeName: String, nodeNo: Long) = Signed("read_model") { implicit request => implicit user =>
    Ok
  }

  def findOneById(typeName: String, nodeId: String) = Signed("read_model") { implicit request => implicit user =>
    Ok
  }

  def add(typeName: String) = Signed("create_model") { implicit request => implicit user =>
    Ok
  }

  def addWithJson(typeName: String, nodeNo: Long) = Signed("update_model") { implicit request => implicit user =>
    Ok
  }

  def delete(typeName: String, nodeNo: Long) = Signed("delete_model") { implicit request => implicit user =>
    Ok
  }
}