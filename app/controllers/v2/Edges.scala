package controllers.v2

import play.api._
import play.api.mvc._
import play.api.cache._
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Json._
import controllers._
import models.{Node, Edge, Type, Order}

object Edges extends Controller with Secured with TypeConversion {
  
  def findAllByTypes(sT: String, v: String, oT: String) = Signed("search_status") { implicit request => implicit user =>
    NotImplemented
  }

  def findAllBySubjectNo(sT: String, sNo: Long, v: String, oT: String) = Signed("search_status") { implicit request => implicit user =>
    Node.findOneByNo(sNo) match {
      case Some(sNode) => {
        val edges = Edge.findAllBySubjectAndTypeNo(sNode, v, Type(oT).no)
        println(edges.length)
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

  def findAllBySubjectId(sT: String, sId: String, v: String, oT: String) = Signed("search_status") { implicit request => implicit user =>
    Node.findOneByTypeNoAndId(Type(sT).no, sId) match {
      case Some(sNode) => {
        val edges = Edge.findAllBySubjectAndTypeNo(sNode, v, Type(oT).no)
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

  def findAllByObjectNo(sT: String, v: String, oT: String, oNo: Long) = Signed("search_status") { implicit request => implicit user =>
    Node.findOneByNo(oNo) match {
      case Some(oNode) => {
        val edges = Edge.findAllByTypeNoAndObject(Type(sT).no, v, oNode)
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

  def findAllByObjectId(sT: String, v: String, oT: String, oId: String) = Signed("search_status") { implicit request => implicit user =>
    Node.findOneByTypeNoAndId(Type(oT).no, oId) match {
      case Some(oNode) => {
        val edges = Edge.findAllByTypeNoAndObject(Type(sT).no, v, oNode)
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
            Callback(Results.Ok, e.toJson)
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
            Cache.getAs[Set[Long]](s"${user.no} edge v:${e.v}") match {
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
      NoContent
    }else{
      // info
      NotFound
    }
  }
}