package controllers.v2

import play.api._
import play.api.mvc._
import play.api.cache._
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.i18n.Messages
import controllers._
import models.{Node, Type, Order}
import java.util.Date

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
        (node.id.length, Node.findOneByTypeNoAndId(node.typeNo, node.id)) match {
          case (len, Some(n)) if len > 0 =>
            Callback(Results.Conflict, n.toTypedJson)
          // case len, None if len > 0 =>
          // case 0, _ =>
          case (_, _) =>
            node.ownerNo = user.no
            node.save match {
              case Some(no: Long) => {
                // respond callback
                Cache.getAs[Set[Long]](s"${user.no} node typeNo:${node.typeNo}") match {
                  case Some(list: Set[Long]) => {
                    list.foreach { orderNo =>
                      Order(Node.findOneByNo(orderNo).get.toTypedJson).prepare(user)
                    }
                  }
                  case None =>
                }
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
    request.body.asJson match {
      case Some(json) => 
        val node = Node(json.as[JsObject])
        Node.findOneByNo(node.no) map { n =>
          n.updatedAt = new Date
          n.id = node.id
          n.json = node.json
          n.save
          // respond callback
          Cache.getAs[Set[Long]](s"${user.no} node typeNo:${node.typeNo}") match {
            case Some(list: Set[Long]) => {
              list.foreach { orderNo =>
                Order(Node.findOneByNo(orderNo).get.toTypedJson).prepare(user)
              }
            }
            case None =>
          }
          // debug: make sure GIGO for test case
          Callback(Results.Created, Node.findOneByNo(n.no).get.toTypedJson)
        } getOrElse {
          add(typeName)(request)
          // warn: trying to update not existing model, so fallback to normal add operation.
          // if explicit mode is true, Callback(NotFound, n.toTypedJson)
        }
      case None => Callback(Results.BadRequest, Json.obj( "message" -> Messages("json.invalid")) )
    }
    
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