package controllers.v2

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.i18n.Messages
import controllers._
import models.{Node, Type}

object Nodes extends Controller with Secured {
  
  def findAllByJson(typeName: String) = Signed("search_model") { implicit request => implicit user =>
    NotImplemented
  }
  
  def findOneByNo(typeName: String, nodeNo: Long) = Signed("read_model") { implicit request => implicit user =>
    Node.findOneByNo(nodeNo) match {
      case Some(n) => {
        // debug
        Callback(Results.Ok, n.toTypedJson)
      }
      case None => {
        // info
        NotFound
      }
    }
  }

  def findOneById(typeName: String, nodeId: String) = Signed("read_model") { implicit request => implicit user =>
    Node.findOneById(nodeId) match {
      case Some(n) => {
        // debug
        Callback(Results.Ok, n.toTypedJson)
      }
      case None => {
        // info
        NotFound
      }
    }
  }

  def add(typeName: String) = Signed("create_model") { implicit request => implicit user =>
    request.body.asJson match { 
      case Some(json) =>
        val node = Node(json.as[JsObject])
        Node.findOneByTypeNoAndId(node.typeNo, node.id) map { n =>
          // debug
          Callback(Results.Conflict, n.toTypedJson)
        } getOrElse {
          node.ownerNo = user.no
          node.save match {
            case Some(no: Long) => {
              // debug
              Callback(Results.Created, Node.findOneByNo(no).get.toTypedJson)
            }
            case None => {
              // error: failed to add new model
              InternalServerError
            }
          }
        }
      case None => Callback(Results.BadRequest, Json.obj( "message" -> Messages("json.invalid")) )
    }
  }

  def addWithJson(typeName: String, nodeNo: Long) = Signed("update_model") { implicit request => implicit user =>
    NotImplemented
  }

  def delete(typeName: String, nodeNo: Long) = Signed("delete_model") { implicit request => implicit user =>
    if(Node.delete(nodeNo)){
      // debug
      NoContent
    }else{
      // info
      NotFound
    }
  }
}