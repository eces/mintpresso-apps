package controllers.v2

import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.i18n.Messages
import controllers._
import models.{Node, Type, Order}
import java.util.Date
import actors._

object Nodes extends Controller with Secured {
  
  def findAllByJson(typeName: String, json: Option[String], offset: Option[Long], limit: Option[Long], newest: Option[String], oldest: Option[String]) = Signed("search_model") { implicit request => implicit user =>
    this.log.trace = "controllers.Nodes.findAllByJson"
    var pureJson = "%"
    json.map { j =>
      try {
        // val temp = Json.parse(j).as[JsObject]
        Json.parse(j).as[JsObject].fields foreach { pair =>
          pureJson += s""""${pair._1}":${pair._2.toString}%"""
        }
      } catch {
        case e: Exception => 
          // error
          Callback(Results.BadRequest, Json.obj("status" -> 400))
      }
    }
    var orderBy = Node.orderBy(newest, oldest)
    var slice = Node.limitBy(offset, limit)
    val nodes = Node.findAllByTypeNoAndJson(Type(typeName).no, pureJson, orderBy, slice)

    this.log.processedAt = new java.util.Date

    if(nodes.length > 0){
      Actors.log ! Debug("node.found="+nodes.length, log, user)
      Callback(Results.Ok, nodes.foldLeft(Json.arr()) { (a, b) => a.append(b.toJson) } )
    }else{
      Actors.log ! Debug("node.found=0", log, user)
      Callback(Results.Ok, Json.arr())
    }
  }
  
  def findOneByNo(typeName: String, nodeNo: Long) = Signed("read_model") { implicit request => implicit user =>
    this.log.trace = "controllers.Nodes.findOneByNo"
    Node.findOneByNo(nodeNo) match {
      case Some(n) => {
        this.log.processedAt = new java.util.Date
        
        if(n.typeName != typeName){
          // warning
          Actors.log ! Warn("node.type.invalid", log, user)
          Callback(Results.NotFound, Json.obj("status" -> 404))
        }else{
          // debug
          Actors.log ! Debug("node.found=1", log, user)
          Callback(Results.Ok, n.toTypedJson)
        }
      }
      case None => {
        // info
        Callback(Results.NotFound, Json.obj("status" -> 404))
      }
    }
  }

  def findOneById(typeName: String, nodeId: String) = Signed("read_model") { implicit request => implicit user =>
    this.log.trace = "controllers.Nodes.findOneById"
    Node.findOneById(nodeId) match {
      case Some(n) => {
        this.log.processedAt = new java.util.Date
        
        if(n.typeName != typeName){
          // warning
          Actors.log ! Warn("node.type.invalid", log, user)
          Callback(Results.NotFound, Json.obj("status" -> 404))
        }else{
          // debug
          Actors.log ! Debug("node.found=1", log, user)
          Callback(Results.Ok, n.toTypedJson)
        }
      }
      case None => {
        // info
        Actors.log ! Info("node.empty", log, user)
        Callback(Results.NotFound, Json.obj("status" -> 404))
      }
    }
  }

  def add(typeName: String) = Signed("create_model") { implicit request => implicit user =>
    this.log.trace = "controllers.Nodes.add"
    request.body.asJson match { 
      case Some(json) =>
        val node = Node(json.as[JsObject])
        (node.id.length, Node.findOneByTypeNoAndId(node.typeNo, node.id)) match {
          case (len, Some(n)) if len > 0 =>
            Actors.log ! Error("node.id.duplicate", log, user)
            Callback(Results.Conflict, n.toTypedJson)
          // case len, None if len > 0 =>
          // case 0, _ =>
          case (_, _) =>
            node.ownerNo = user.no
            node.save match {
              case Some(no: Long) => {
                this.log.processedAt = new java.util.Date
                // debug
                Actors.log ! Debug("node.add", log, user)
                node.callback
                Callback(Results.Created, Node.findOneByNo(no).get.toTypedJson)
              }
              case None => {
                this.log.processedAt = new java.util.Date
                // error: failed to add new model
                Actors.log ! Error("node.add.fail", log, user)
                InternalServerError
              }
            }
        }
      case None => 
        // warn
        Actors.log ! Warn("json.invalid", log, user)
        Callback(Results.BadRequest, Json.obj( "message" -> Messages("json.invalid")) )
    }
  }

  def addWithJson(typeName: String, nodeNo: Long) = Signed("update_model") { implicit request => implicit user =>
    this.log.trace = "controllers.Nodes.addWithJson"
    request.body.asJson match {
      case Some(json) => 
        val node = Node(json.as[JsObject])
        Node.findOneByNo(node.no) map { n =>
          n.updatedAt = new Date
          n.id = node.id
          n.json = node.json
          n.save

          this.log.processedAt = new java.util.Date
          Actors.log ! Debug("node.addWithJson", log, user)
          
          // respond callback
          n.callback
          
          // debug: make sure GIGO for test case
          Callback(Results.Created, Node.findOneByNo(n.no).get.toTypedJson)
        } getOrElse {
          // warn: trying to update not existing model, so fallback to normal add operation.
          Actors.log ! Warn("node.update.empty", log, user)
          add(typeName)(request)
          // if explicit mode is true, Callback(NotFound, n.toTypedJson)
        }
      case None => 
        // warn
        Actors.log ! Warn("json.invalid", log, user)
        Callback(Results.BadRequest, Json.obj( "message" -> Messages("json.invalid")) )
    }
    
  }

  def delete(typeName: String, nodeNo: Long) = Signed("delete_model") { implicit request => implicit user =>
    this.log.trace = "controllers.Nodes.delete"
    if(Node.delete(nodeNo)){
      this.log.processedAt = new java.util.Date
      // debug
      Actors.log ! Warn("node.delete", log, user)
      Accepted
    }else{
      this.log.processedAt = new java.util.Date
      // info
      Actors.log ! Info("node.delete.fail", log, user)
      NoContent
    }
  }
}