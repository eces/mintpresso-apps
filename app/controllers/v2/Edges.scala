package controllers.v2

import play.api._
import play.api.mvc._
import play.api.cache._
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Json._
import controllers._
import models.{Node, Edge, Type, Order}
import actors._

object Edges extends Controller with Secured with TypeConversion {
  def findAllByTypes(sT: String, v: String, oT: String, offset: Option[Long], limit: Option[Long], newest: Option[String], oldest: Option[String]) = Signed("search_status") { implicit request => implicit user =>
    var orderBy = Edge.orderBy(newest, oldest)
    var slice = Edge.limitBy(offset, limit)
    val edges = Edge.findAllByTypes(Type(sT).no, v, Type(oT).no, orderBy, slice)
    this.log.trace = "controllers.Edges.findAllByTypes"
    this.log.processedAt = new java.util.Date
    if(edges.length > 0){
      Actors.log ! Debug("edge.found="+edges.length, log, user)
      Callback(Results.Ok, edges.foldLeft(Json.arr()) { (a, b) => a.append(b.toJson) } )
    }else{
      Actors.log ! Debug("edge.found=0", log, user)
      Callback(Results.Ok, Json.arr())
    }
  }

  def findAllBySubjectNo(sT: String, sNo: Long, v: String, oT: String, offset: Option[Long], limit: Option[Long], newest: Option[String], oldest: Option[String]) = Signed("search_status") { implicit request => implicit user =>
    Node.findOneByNo(sNo) match {
      case Some(sNode) => {
        var orderBy = Edge.orderBy(newest, oldest)
        var slice = Edge.limitBy(offset, limit)
        val edges = Edge.findAllBySubjectAndTypeNo(sNode, v, Type(oT).no, orderBy, slice)
        if(edges.length > 0){
          Callback(Results.Ok, edges.foldLeft(Json.arr()) { (a, b) => a.append(b.toJson) } )
        }else{
          Callback(Results.Ok, Json.arr())
        }
      }
      case None => {
        // info
        NotFound
      }
    }
  }

  def findAllBySubjectId(sT: String, sId: String, v: String, oT: String, offset: Option[Long], limit: Option[Long], newest: Option[String], oldest: Option[String]) = Signed("search_status") { implicit request => implicit user =>
    Node.findOneByTypeNoAndId(Type(sT).no, sId) match {
      case Some(sNode) => {
        var orderBy = Edge.orderBy(newest, oldest)
        var slice = Edge.limitBy(offset, limit)
        val edges = Edge.findAllBySubjectAndTypeNo(sNode, v, Type(oT).no, orderBy, slice)
        if(edges.length > 0){
          Callback(Results.Ok, edges.foldLeft(Json.arr()) { (a, b) => a.append(b.toJson) } )
        }else{
          Callback(Results.Ok, Json.arr())
        }
      }
      case None => {
        // info
        NotFound
      }
    }
  }

  def findAllByObjectNo(sT: String, v: String, oT: String, oNo: Long, offset: Option[Long], limit: Option[Long], newest: Option[String], oldest: Option[String]) = Signed("search_status") { implicit request => implicit user =>
    Node.findOneByNo(oNo) match {
      case Some(oNode) => {
        var orderBy = Edge.orderBy(newest, oldest)
        var slice = Edge.limitBy(offset, limit)
        val edges = Edge.findAllByTypeNoAndObject(Type(sT).no, v, oNode, orderBy, slice)
        if(edges.length > 0){
          Callback(Results.Ok, edges.foldLeft(Json.arr()) { (a, b) => a.append(b.toJson) } )
        }else{
          Callback(Results.Ok, Json.arr())
        }
      }
      case None => {
        // info
        NotFound
      }
    }
  }

  def findAllByObjectId(sT: String, v: String, oT: String, oId: String, offset: Option[Long], limit: Option[Long], newest: Option[String], oldest: Option[String]) = Signed("search_status") { implicit request => implicit user =>
    Node.findOneByTypeNoAndId(Type(oT).no, oId) match {
      case Some(oNode) => {
        var orderBy = Edge.orderBy(newest, oldest)
        var slice = Edge.limitBy(offset, limit)
        val edges = Edge.findAllByTypeNoAndObject(Type(sT).no, v, oNode, orderBy, slice)
        if(edges.length > 0){
          Callback(Results.Ok, edges.foldLeft(Json.arr()) { (a, b) => a.append(b.toJson) } )
        }else{
          Callback(Results.Ok, Json.arr())
        }
      }
      case None => {
        // info
        NotFound
      }
    }
  }

  def findOne(sT: String, s: String, v: String, oT: String, o: String) = Signed("search_status") { implicit request => implicit user =>

    var sNode: Option[Node] = None
    var oNode: Option[Node] = None

    (s, o) match {
      case (Long(sNo), Long(oNo)) => {
        sNode = Node.findOneByNo(sNo)
        oNode = Node.findOneByNo(oNo)
      }
      case (sId, Long(oNo)) => {
        sNode = Node.findOneByTypeNoAndId(Type(sT).no, sId)
        oNode = Node.findOneByNo(oNo)
      }
      case (Long(sNo), oId) => {
        sNode = Node.findOneByNo(sNo)
        oNode = Node.findOneByTypeNoAndId(Type(oT).no, oId)
      }
      case (sId, oId) => {
        sNode = Node.findOneByTypeNoAndId(Type(sT).no, sId)
        oNode = Node.findOneByTypeNoAndId(Type(oT).no, oId)
      }
    }

    (sNode, oNode) match {
      case (Some(sNode), Some(oNode)) => {
        Edge.findOneWith(sNode, v, oNode) match {
          case Some(e) => 
            if(sNode.typeName != sT || oNode.typeName != oT){
              // warning
              NotFound
            }else{
              // debug
              Callback(Results.Ok, e.toJson)
            }
          case None =>
            NotFound
        }
      }
      case (Some(sNode), None) => {
        // info
        Status(422)
      }
      case (None, Some(oNode)) => {
        // info
        Status(422)
      }
      case (None, None) => {
        // info
        Status(422)
      }
    }
  }

  def add(sT: String, s: String, v: String, oT: String, o: String) = Signed("create_status") { implicit request => implicit user =>
    var sNode: Option[Node] = None
    var oNode: Option[Node] = None

    (s, o) match {
      case (Long(sNo), Long(oNo)) => {
        sNode = Node.findOneByNo(sNo)
        oNode = Node.findOneByNo(oNo)
      }
      case (sId, Long(oNo)) => {
        sNode = Node.findOneByTypeNoAndId(Type(sT).no, sId)
        oNode = Node.findOneByNo(oNo)
      }
      case (Long(sNo), oId) => {
        sNode = Node.findOneByNo(sNo)
        oNode = Node.findOneByTypeNoAndId(Type(oT).no, oId)
      }
      case (sId, oId) => {
        sNode = Node.findOneByTypeNoAndId(Type(sT).no, sId)
        oNode = Node.findOneByTypeNoAndId(Type(oT).no, oId)
      }
    }

    (sNode, oNode) match {
      case (Some(sNode), Some(oNode)) => {

        var json: Option[JsValue] = None
        var parsed = false

        // Content-Length determines whether do parse or not.
        val cl = request.headers("Content-Length")
        if(cl.length != 0 && cl != "0"){
          json = request.body.asJson
          parsed = true
        }
        
        val e = Edge( 0L, user.no, sNode, v, oNode )

        json match {
          case Some(jsValue) => e.json = jsValue.as[JsObject]
          case None if parsed => {
            // warn
          }
          case _ =>
        }
        
        e.save match {
          case Some(no: Long) => {
            // respond callback
            e.callback
            // debug
            Callback(Results.Created, Edge.findOneByNo(no).get.toJson)
          }
          case None => {
            // error: failed to add new status
            InternalServerError
          }
        }  
      }
      case (Some(sNode), None) => {
        // info
        Status(422)
      }
      case (None, Some(oNode)) => {
        // info
        Status(422)
      }
      case (None, None) => {
        // info
        Status(422)
      }
    }
  }

  def delete(no: Long) = Signed("delete_status") { implicit request => implicit user =>
    if(Edge.deleteByNo(no)){
      // debug
      Accepted
    }else{
      // info
      NoContent
    }
  }
}